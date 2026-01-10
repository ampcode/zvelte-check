//! tsgo runner - spawns tsgo for TypeScript type-checking
//!
//! Writes transformed .svelte.ts files alongside their .svelte sources,
//! generates a tsconfig that extends the project's config, runs tsgo
//! from the workspace root for proper module resolution, then cleans up.

const std = @import("std");
const builtin = @import("builtin");
const VirtualFile = @import("transformer.zig").VirtualFile;
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const SourceMap = @import("source_map.zig").SourceMap;
const LineTable = @import("source_map.zig").LineTable;

const stub_dir = ".zvelte-check";
const generated_tsconfig = stub_dir ++ "/tsconfig.json";
const generated_stubs = stub_dir ++ "/stubs.d.ts";

pub const TsgoNotFoundError = error{TsgoNotFound};

/// VirtualFile with pre-computed LineTables for efficient diagnostic mapping.
/// Avoids O(N × file_size) rebuilding of line tables per diagnostic.
const CachedVirtualFile = struct {
    vf: VirtualFile,
    ts_line_table: LineTable,
    svelte_line_table: LineTable,
};

/// Platform string for @typescript/native-preview package resolution.
/// Maps Zig's builtin OS/arch to npm's platform naming convention.
const native_preview_platform: ?[]const u8 = blk: {
    const os = switch (builtin.os.tag) {
        .macos => "darwin",
        .linux => "linux",
        .windows => "win32",
        else => break :blk null,
    };
    const arch = switch (builtin.cpu.arch) {
        .aarch64 => "arm64",
        .x86_64 => "x64",
        .arm => "arm",
        else => break :blk null,
    };
    break :blk os ++ "-" ++ arch;
};

/// Finds the tsgo binary. Tries in order:
/// 1. Native binary directly: node_modules/@typescript/native-preview-{platform}/lib/tsgo (fastest)
/// 2. Sibling of main package (pnpm): resolve symlink, check ../native-preview-{platform}/lib/tsgo
/// 3. Bin wrapper: node_modules/.bin/tsgo (slower, spawns Node.js)
/// 4. PATH: "tsgo" (fallback)
fn findTsgoBinary(allocator: std.mem.Allocator, workspace_path: []const u8) ![]const u8 {
    // Resolve to absolute path so we can walk up parent directories
    const abs_path = std.fs.cwd().realpathAlloc(allocator, workspace_path) catch {
        return "tsgo";
    };
    defer allocator.free(abs_path);

    var search_path: []const u8 = abs_path;

    while (true) {
        if (native_preview_platform) |platform| {
            // Fast path 1: flat node_modules (npm, yarn)
            const native_path = std.fs.path.join(allocator, &.{
                search_path,
                "node_modules/@typescript/native-preview-" ++ platform ++ "/lib/tsgo",
            }) catch break;

            if (std.fs.cwd().access(native_path, .{})) {
                return native_path;
            } else |_| {
                allocator.free(native_path);
            }

            // Fast path 2: pnpm - resolve symlink and check sibling package
            // pnpm stores optionalDeps as siblings in .pnpm store:
            // .pnpm/@typescript+native-preview@.../node_modules/@typescript/native-preview
            // .pnpm/@typescript+native-preview-darwin-arm64@.../node_modules/@typescript/native-preview-darwin-arm64
            // The main package has a symlink to the sibling: ../native-preview-{platform}
            const main_pkg_path = std.fs.path.join(allocator, &.{
                search_path,
                "node_modules/@typescript/native-preview",
            }) catch break;
            defer allocator.free(main_pkg_path);

            // Resolve symlink to get real path in .pnpm store
            if (std.fs.cwd().realpathAlloc(allocator, main_pkg_path)) |real_main_path| {
                defer allocator.free(real_main_path);
                // Go up one level (@typescript/) and find sibling platform package
                if (std.fs.path.dirname(real_main_path)) |parent_dir| {
                    const sibling_path = std.fs.path.join(allocator, &.{
                        parent_dir,
                        "native-preview-" ++ platform ++ "/lib/tsgo",
                    }) catch break;

                    if (std.fs.cwd().access(sibling_path, .{})) {
                        return sibling_path;
                    } else |_| {
                        allocator.free(sibling_path);
                    }
                }
            } else |_| {}
        }

        // Slow path: try .bin wrapper (spawns Node.js to resolve and exec binary)
        const bin_path = std.fs.path.join(allocator, &.{ search_path, "node_modules/.bin/tsgo" }) catch {
            break;
        };

        if (std.fs.cwd().access(bin_path, .{})) {
            return bin_path;
        } else |_| {
            allocator.free(bin_path);
        }

        // Move to parent directory
        const parent = std.fs.path.dirname(search_path);
        if (parent == null or std.mem.eql(u8, parent.?, search_path)) {
            break;
        }
        search_path = parent.?;
    }

    // Fall back to tsgo in PATH
    return "tsgo";
}

pub fn check(
    allocator: std.mem.Allocator,
    virtual_files: []const VirtualFile,
    workspace_path: []const u8,
    tsconfig_path: ?[]const u8,
) ![]Diagnostic {
    if (virtual_files.len == 0) return &.{};

    // Open workspace directory for writing generated files
    var workspace_dir = try std.fs.cwd().openDir(workspace_path, .{});
    defer workspace_dir.close();

    // Create stub directory for all generated files (avoids polluting source tree)
    try workspace_dir.makePath(stub_dir);
    errdefer cleanupStubDir(workspace_dir);

    // Write transformed .svelte.ts files into stub directory
    for (virtual_files) |vf| {
        // Strip workspace prefix from virtual_path to get relative path
        const relative_path = stripWorkspacePrefix(vf.virtual_path, workspace_path);

        // Write to stub_dir/<relative_path> (e.g., .zvelte-check/src/routes/+page.svelte.ts)
        const stub_path = try std.fs.path.join(allocator, &.{ stub_dir, relative_path });
        defer allocator.free(stub_path);
        try writeVirtualFile(workspace_dir, stub_path, vf.content);
    }

    // Write SvelteKit ambient type stubs
    try writeSvelteKitStubs(workspace_dir);

    // Generate tsconfig that extends project config and includes only our files
    try writeGeneratedTsconfig(workspace_dir, workspace_path, tsconfig_path, virtual_files);

    // Find tsgo binary: walk up from workspace looking for node_modules/.bin, then PATH
    const tsgo_path = try findTsgoBinary(allocator, workspace_path);

    // Build tsgo command
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    try args.append(allocator, tsgo_path);
    try args.append(allocator, "--noEmit");
    try args.append(allocator, "--project");

    const tsconfig_abs = try std.fs.path.join(allocator, &.{ workspace_path, generated_tsconfig });
    defer allocator.free(tsconfig_abs);
    try args.append(allocator, tsconfig_abs);

    // Run tsgo from workspace root for proper module resolution
    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.cwd = workspace_path;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            return TsgoNotFoundError.TsgoNotFound;
        }
        return err;
    };

    // Collect stdout/stderr concurrently to avoid pipe deadlock
    var stdout_buf: std.ArrayList(u8) = .empty;
    defer stdout_buf.deinit(allocator);
    var stderr_buf: std.ArrayList(u8) = .empty;
    defer stderr_buf.deinit(allocator);

    try child.collectOutput(allocator, &stdout_buf, &stderr_buf, 10 * 1024 * 1024);

    // Wait can also return FileNotFound if spawn was deferred
    const result = child.wait() catch |err| {
        if (err == error.FileNotFound) {
            return TsgoNotFoundError.TsgoNotFound;
        }
        return err;
    };

    const stdout = stdout_buf.items;
    const stderr = stderr_buf.items;

    // Clean up stub directory before parsing output
    cleanupStubDir(workspace_dir);

    // Parse tsgo output
    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    const has_output = stdout.len > 0 or stderr.len > 0;
    const exited_with_error = switch (result) {
        .Exited => |code| code != 0,
        else => true,
    };

    if (exited_with_error or has_output) {
        // Pre-compute line tables for all virtual files once, avoiding O(N × file_size)
        // rebuilding per diagnostic line
        var cached_files: std.ArrayList(CachedVirtualFile) = .empty;
        defer cached_files.deinit(allocator);

        for (virtual_files) |vf| {
            try cached_files.append(allocator, .{
                .vf = vf,
                .ts_line_table = try LineTable.init(allocator, vf.content),
                .svelte_line_table = try LineTable.init(allocator, vf.source_map.svelte_source),
            });
        }

        try parseTsgoOutput(allocator, stdout, cached_files.items, &diagnostics);
        if (stderr.len > 0) {
            try parseTsgoOutput(allocator, stderr, cached_files.items, &diagnostics);
        }
    }

    return diagnostics.toOwnedSlice(allocator);
}

/// Writes a transformed .svelte.ts file to the stub directory.
/// `relative_path` should be relative to workspace_dir (e.g., .zvelte-check/src/routes/+page.svelte.ts).
fn writeVirtualFile(workspace_dir: std.fs.Dir, relative_path: []const u8, content: []const u8) !void {
    // Create parent directories if needed (for nested paths like src/routes/+page.svelte.ts)
    if (std.fs.path.dirname(relative_path)) |parent| {
        try workspace_dir.makePath(parent);
    }
    const file = try workspace_dir.createFile(relative_path, .{});
    defer file.close();
    try file.writeAll(content);
}

/// Strips workspace prefix from a path to get a relative path.
/// If workspace_path is empty or path doesn't start with it, returns the original path.
fn stripWorkspacePrefix(path: []const u8, workspace_path: []const u8) []const u8 {
    if (workspace_path.len == 0) return path;
    if (!std.mem.startsWith(u8, path, workspace_path)) return path;

    // Calculate how much to skip: workspace path + separator
    var skip_len = workspace_path.len;
    if (!std.mem.endsWith(u8, workspace_path, "/") and skip_len < path.len and path[skip_len] == '/') {
        skip_len += 1;
    }

    return if (skip_len < path.len) path[skip_len..] else path;
}

/// Cleans up the stub directory containing all generated files.
fn cleanupStubDir(workspace_dir: std.fs.Dir) void {
    workspace_dir.deleteTree(stub_dir) catch {};
}

/// Generates a tsconfig that extends the project's config.
/// Includes only transformed .svelte.ts files and our type stubs.
fn writeGeneratedTsconfig(
    workspace_dir: std.fs.Dir,
    workspace_path: []const u8,
    tsconfig_path: ?[]const u8,
    virtual_files: []const VirtualFile,
) !void {
    const file = try workspace_dir.createFile(generated_tsconfig, .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    var writer = file.writer(&buf);
    errdefer writer.end() catch {};
    const w = &writer.interface;

    try w.writeAll("{\n");

    // Extend project tsconfig if available (for path aliases, baseUrl, etc.)
    // Use ../ since our tsconfig is in .zvelte-check/ subdirectory
    if (tsconfig_path) |ptsconfig| {
        try w.writeAll("  \"extends\": \"../");
        try w.writeAll(ptsconfig);
        try w.writeAll("\",\n");
    } else {
        // Auto-detect tsconfig.json in workspace root
        if (workspace_dir.access("tsconfig.json", .{})) {
            try w.writeAll("  \"extends\": \"../tsconfig.json\",\n");
        } else |_| {}
    }

    // Compiler options optimized for Svelte checking
    // Disable strict null checks and implicit any since Svelte's reactivity patterns
    // often produce false positives (e.g., $state(null) with later narrowing, or
    // callback parameters that Svelte infers at runtime).
    try w.writeAll("  \"compilerOptions\": {\n");
    try w.writeAll("    \"noEmit\": true,\n");
    try w.writeAll("    \"skipLibCheck\": true,\n");
    try w.writeAll("    \"allowJs\": true,\n");
    try w.writeAll("    \"checkJs\": true,\n");
    try w.writeAll("    \"noUnusedLocals\": false,\n");
    try w.writeAll("    \"noUnusedParameters\": false,\n");
    try w.writeAll("    \"strictNullChecks\": false,\n");
    try w.writeAll("    \"noImplicitAny\": false\n");
    try w.writeAll("  },\n");

    // Include our generated files plus all .ts files they may import
    // Paths are relative to this tsconfig in .zvelte-check/
    try w.writeAll("  \"include\": [\n");
    try w.writeAll("    \"stubs.d.ts\",\n");
    try w.writeAll("    \"../src/**/*.ts\"");

    for (virtual_files) |vf| {
        // Strip workspace prefix from virtual_path to get relative path
        const relative_path = stripWorkspacePrefix(vf.virtual_path, workspace_path);

        // Include the stub file (already in .zvelte-check/ relative to tsconfig)
        try w.writeAll(",\n    \"");
        try w.writeAll(relative_path);
        try w.writeAll("\"");
    }

    try w.writeAll("\n  ],\n");

    // Exclude node_modules to avoid scanning everything
    try w.writeAll("  \"exclude\": [\"../node_modules\"]\n");
    try w.writeAll("}\n");

    try w.flush();
}

/// Writes SvelteKit virtual module type stubs to the workspace.
/// These ambient declarations allow tsgo to resolve $app/* imports.
fn writeSvelteKitStubs(workspace_dir: std.fs.Dir) !void {
    const file = try workspace_dir.createFile(generated_stubs, .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    var writer = file.writer(&buf);
    errdefer writer.end() catch {};

    try writer.interface.writeAll(
        \\// SvelteKit virtual module type stubs
        \\// Auto-generated by zvelte-check
        \\
        \\declare module "$app/environment" {
        \\  export const browser: boolean;
        \\  export const dev: boolean;
        \\  export const building: boolean;
        \\  export const version: string;
        \\}
        \\
        \\declare module "$app/stores" {
        \\  import type { Readable } from "svelte/store";
        \\  export const page: Readable<{
        \\    url: URL;
        \\    params: Record<string, string>;
        \\    route: { id: string | null };
        \\    status: number;
        \\    error: Error | null;
        \\    data: Record<string, any>;
        \\    state: Record<string, any>;
        \\    form: any;
        \\  }>;
        \\  export const navigating: Readable<{
        \\    from: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    to: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    type: 'load' | 'unload' | 'link' | 'goto' | 'popstate';
        \\    willUnload: boolean;
        \\    delta?: number;
        \\    complete: Promise<void>;
        \\  } | null>;
        \\  export const updated: Readable<boolean> & { check: () => Promise<boolean> };
        \\}
        \\
        \\declare module "$app/navigation" {
        \\  export function goto(url: string | URL, opts?: {
        \\    replaceState?: boolean;
        \\    noScroll?: boolean;
        \\    keepFocus?: boolean;
        \\    invalidateAll?: boolean;
        \\    state?: any;
        \\  }): Promise<void>;
        \\  export function invalidate(url: string | URL | ((url: URL) => boolean)): Promise<void>;
        \\  export function invalidateAll(): Promise<void>;
        \\  export function preloadData(url: string | URL): Promise<Record<string, any>>;
        \\  export function preloadCode(...urls: string[]): Promise<void>;
        \\  export function beforeNavigate(callback: (navigation: {
        \\    from: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    to: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    willUnload: boolean;
        \\    type: 'load' | 'unload' | 'link' | 'goto' | 'popstate';
        \\    cancel: () => void;
        \\  }) => void): void;
        \\  export function afterNavigate(callback: (navigation: {
        \\    from: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    to: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    type: 'load' | 'unload' | 'link' | 'goto' | 'popstate';
        \\    willUnload: boolean;
        \\  }) => void): void;
        \\  export function onNavigate(callback: (navigation: {
        \\    from: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    to: { url: URL; params: Record<string, string>; route: { id: string | null } };
        \\    type: 'link' | 'goto' | 'popstate';
        \\  }) => void | (() => void) | Promise<void | (() => void)>): void;
        \\  export function disableScrollHandling(): void;
        \\  export function pushState(url: string | URL, state: any): void;
        \\  export function replaceState(url: string | URL, state: any): void;
        \\}
        \\
        \\declare module "$app/forms" {
        \\  export function enhance<Success = Record<string, any>, Failure = Record<string, any>>(
        \\    form: HTMLFormElement,
        \\    options?: {
        \\      formElement?: HTMLFormElement;
        \\      formData?: FormData;
        \\      action?: URL;
        \\      submitter?: HTMLElement | null;
        \\      cancel?: () => void;
        \\      controller?: AbortController;
        \\    } | ((input: { formElement: HTMLFormElement; formData: FormData; action: URL; submitter: HTMLElement | null; cancel: () => void; controller: AbortController }) => Promise<void | ((opts: { formElement: HTMLFormElement; formData: FormData; action: URL; result: { type: 'success' | 'failure' | 'redirect' | 'error'; status?: number; data?: Success | Failure; location?: string; error?: any }; update: (options?: { reset?: boolean; invalidateAll?: boolean }) => Promise<void>; }) => Promise<void>)>)
        \\  ): { destroy: () => void };
        \\  export function applyAction<T = Record<string, any>>(result: { type: 'success' | 'failure' | 'redirect' | 'error'; status?: number; data?: T; location?: string; error?: any }): Promise<void>;
        \\}
        \\
        \\declare module "$app/paths" {
        \\  export const base: string;
        \\  export const assets: string;
        \\  export function resolveRoute(id: string, params: Record<string, string | undefined>): string;
        \\}
        \\
        \\declare module "$app/state" {
        \\  export const page: {
        \\    readonly url: URL;
        \\    readonly params: Record<string, string>;
        \\    readonly route: { readonly id: string | null };
        \\    readonly status: number;
        \\    readonly error: Error | null;
        \\    readonly data: Record<string, any>;
        \\    readonly state: Record<string, any>;
        \\    readonly form: any;
        \\  };
        \\  export const navigating: {
        \\    readonly from: { readonly url: URL; readonly params: Record<string, string>; readonly route: { readonly id: string | null } } | null;
        \\    readonly to: { readonly url: URL; readonly params: Record<string, string>; readonly route: { readonly id: string | null } } | null;
        \\    readonly type: 'load' | 'unload' | 'link' | 'goto' | 'popstate';
        \\    readonly willUnload: boolean;
        \\    readonly delta?: number;
        \\    readonly complete: Promise<void>;
        \\  } | null;
        \\  export const updated: {
        \\    readonly current: boolean;
        \\    check: () => Promise<boolean>;
        \\  };
        \\}
        \\
        \\declare module "$app/server" {
        \\  export function read(asset: string): Response;
        \\}
        \\
        \\// Generic .svelte file type declaration
        \\// Allows tsgo to resolve imports like `import Component from './Button.svelte'`
        \\// Named exports from .svelte files are typed via their generated .svelte.ts files
        \\declare module "*.svelte" {
        \\  import { SvelteComponent } from "svelte";
        \\  const component: typeof SvelteComponent<any, any, any>;
        \\  export default component;
        \\}
        \\
        \\// unplugin-icons virtual module stubs
        \\// Allows tsgo to resolve imports like `import Icon from '~icons/lucide/check'`
        \\declare module "~icons/*" {
        \\  import { SvelteComponent } from "svelte";
        \\  const component: typeof SvelteComponent;
        \\  export default component;
        \\}
        \\
        \\declare module "$app/types" {
        \\  // Re-export common SvelteKit types
        \\  export type Navigation = {
        \\    from: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    to: { url: URL; params: Record<string, string>; route: { id: string | null } } | null;
        \\    type: 'load' | 'unload' | 'link' | 'goto' | 'popstate';
        \\    willUnload: boolean;
        \\    delta?: number;
        \\    complete: Promise<void>;
        \\  };
        \\  export type Page<Data = Record<string, any>> = {
        \\    url: URL;
        \\    params: Record<string, string>;
        \\    route: { id: string | null };
        \\    status: number;
        \\    error: Error | null;
        \\    data: Data;
        \\    state: Record<string, any>;
        \\    form: any;
        \\  };
        \\}
        \\
        \\// SvelteKit $env/* virtual module types
        \\declare module "$env/static/private" {
        \\  const env: Record<string, string>;
        \\  export = env;
        \\}
        \\
        \\declare module "$env/static/public" {
        \\  const env: Record<string, string>;
        \\  export = env;
        \\}
        \\
        \\declare module "$env/dynamic/private" {
        \\  export const env: Record<string, string | undefined>;
        \\}
        \\
        \\declare module "$env/dynamic/public" {
        \\  export const env: Record<string, string | undefined>;
        \\}
        \\
        \\// Vite import.meta.env ambient types
        \\interface ImportMetaEnv {
        \\  readonly MODE: string;
        \\  readonly BASE_URL: string;
        \\  readonly PROD: boolean;
        \\  readonly DEV: boolean;
        \\  readonly SSR: boolean;
        \\  [key: string]: any;
        \\}
        \\
        \\interface ImportMeta {
        \\  readonly env: ImportMetaEnv;
        \\}
        \\
        \\// Note: SvelteKit ./$types resolution relies on the generated
        \\// .svelte-kit/types/*/$types.d.ts files. Users should run `pnpm dev`
        \\// or `pnpm build` once to generate these types.
        \\
        \\// SvelteKit App namespace for locals and platform
        \\declare namespace App {
        \\  interface Locals {}
        \\  interface Platform {}
        \\  interface PageState {}
        \\}
        \\
    );

    try writer.interface.flush();
}

fn parseTsgoOutput(
    allocator: std.mem.Allocator,
    output: []const u8,
    cached_files: []const CachedVirtualFile,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // tsgo output format: file(line,col): error TSxxxx: message
    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse: filename(line,col): severity TScode: message
        const paren_start = std.mem.indexOf(u8, line, "(") orelse continue;
        const paren_end = std.mem.indexOf(u8, line, ")") orelse continue;
        const colon_pos = std.mem.indexOf(u8, line[paren_end..], ":") orelse continue;

        const file_path = line[0..paren_start];
        const pos_str = line[paren_start + 1 .. paren_end];
        const rest = line[paren_end + colon_pos + 1 ..];

        // Parse line,col (1-based from tsgo)
        var pos_parts = std.mem.splitScalar(u8, pos_str, ',');
        const line_str = pos_parts.next() orelse continue;
        const col_str = pos_parts.next() orelse continue;

        const ts_line = std.fmt.parseInt(u32, line_str, 10) catch continue;
        const ts_col = std.fmt.parseInt(u32, col_str, 10) catch continue;

        // Find corresponding cached file by matching .svelte.ts path
        const cached: ?CachedVirtualFile = for (cached_files) |cf| {
            if (std.mem.endsWith(u8, file_path, cf.vf.virtual_path)) {
                break cf;
            }
        } else null;

        // Only report errors from .svelte.ts files we generated.
        // Skip errors from other .ts files that tsgo checked transitively.
        if (cached == null) continue;

        // Map TS position back to Svelte position
        var svelte_line = ts_line;
        var svelte_col = ts_col;
        var original_path: []const u8 = file_path;

        if (cached) |cf| {
            original_path = cf.vf.original_path;

            // Use pre-computed line table to convert line/col → offset
            if (cf.ts_line_table.lineColToOffset(ts_line, ts_col)) |ts_offset| {
                // Map TS offset to Svelte offset
                if (cf.vf.source_map.tsToSvelte(ts_offset)) |svelte_offset| {
                    // Use pre-computed Svelte line table to convert offset → line/col
                    const pos = cf.svelte_line_table.offsetToLineCol(svelte_offset);
                    // Convert from 0-based to 1-based
                    svelte_line = pos.line + 1;
                    svelte_col = pos.col + 1;
                }
            }
        }

        // Parse severity and message
        const trimmed = std.mem.trim(u8, rest, " ");
        const is_error = std.mem.startsWith(u8, trimmed, "error");

        // Find message after code
        const msg_start = std.mem.indexOf(u8, trimmed, ": ");
        const message = if (msg_start) |s| trimmed[s + 2 ..] else trimmed;

        // Skip errors about Svelte types that users import themselves
        // These appear when module scripts reference types before user imports
        if (shouldSkipSvelteTypeError(message)) continue;

        try diagnostics.append(allocator, .{
            .source = .js,
            .severity = if (is_error) .@"error" else .warning,
            .code = null,
            .message = try allocator.dupe(u8, message),
            .file_path = try allocator.dupe(u8, original_path),
            .start_line = svelte_line,
            .start_col = svelte_col,
            .end_line = svelte_line,
            .end_col = svelte_col,
        });
    }
}

/// Returns true if the error message is about a Svelte type that users
/// typically import themselves. These errors occur when module scripts
/// reference Svelte types (like Component, ComponentProps) that are
/// imported in the instance script, which appears later in generated code.
fn shouldSkipSvelteTypeError(message: []const u8) bool {
    // Svelte types that users commonly import and use in module scripts
    const svelte_types = [_][]const u8{
        "Cannot find name 'Component'.",
        "Cannot find name 'ComponentProps'.",
        "Cannot find name 'ComponentType'.",
        "Cannot find name 'SvelteComponent'.",
    };

    for (svelte_types) |pattern| {
        if (std.mem.eql(u8, message, pattern)) return true;
    }

    // Skip "Property X does not exist on type 'never'" errors
    // These are false positives from $state(null) patterns where TypeScript
    // incorrectly narrows the type to 'never' after truthiness checks in
    // closures like $effect or onMount callbacks.
    if (std.mem.indexOf(u8, message, "does not exist on type 'never'") != null) {
        return true;
    }

    // Skip "Variable X is used before being assigned" errors
    // These are false positives from bind:this patterns where variables
    // are declared without initializers but get assigned by Svelte's
    // runtime binding system (e.g., let canvas: HTMLCanvasElement with bind:this={canvas})
    if (std.mem.indexOf(u8, message, "is used before being assigned") != null) {
        return true;
    }

    return false;
}

test "parse tsgo output" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tsgo_output = "src/routes/+page.svelte.ts(5,10): error TS2322: Type 'string' is not assignable to type 'number'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    const vf: VirtualFile = .{
        .original_path = "src/routes/+page.svelte",
        .virtual_path = "src/routes/+page.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    };

    const cached_files = [_]CachedVirtualFile{.{
        .vf = vf,
        .ts_line_table = try LineTable.init(allocator, vf.content),
        .svelte_line_table = try LineTable.init(allocator, vf.source_map.svelte_source),
    }};

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("src/routes/+page.svelte", diagnostics.items[0].file_path);
}

test "writeGeneratedTsconfig generates valid config" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp workspace dir to test
    var workspace_dir = try std.fs.cwd().makeOpenPath(".zvelte-check-test-workspace", .{});
    defer {
        workspace_dir.close();
        std.fs.cwd().deleteTree(".zvelte-check-test-workspace") catch {};
    }

    // Create the stub directory (normally done by check())
    try workspace_dir.makePath(stub_dir);

    const virtual_files = [_]VirtualFile{.{
        .original_path = "src/routes/+page.svelte",
        .virtual_path = "src/routes/+page.svelte.ts",
        .content = "const x = 1;",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    // Test with no project tsconfig (empty string as workspace path since paths are already relative)
    try writeGeneratedTsconfig(workspace_dir, "", null, &virtual_files);

    // Read generated config
    const content = try workspace_dir.readFileAlloc(allocator, generated_tsconfig, 10 * 1024);

    // Verify basic structure
    try std.testing.expect(std.mem.indexOf(u8, content, "\"compilerOptions\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"include\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "src/routes/+page.svelte.ts") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"exclude\"") != null);
    // Verify .ts files are included for imports from .svelte files (with ../ prefix)
    try std.testing.expect(std.mem.indexOf(u8, content, "\"../src/**/*.ts\"") != null);
}

test "writeGeneratedTsconfig extends project config when available" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp workspace dir to test
    var workspace_dir = try std.fs.cwd().makeOpenPath(".zvelte-check-test-workspace", .{});
    defer {
        workspace_dir.close();
        std.fs.cwd().deleteTree(".zvelte-check-test-workspace") catch {};
    }

    // Create the stub directory (normally done by check())
    try workspace_dir.makePath(stub_dir);

    // Create a fake project tsconfig
    const tsconfig_file = try workspace_dir.createFile("tsconfig.json", .{});
    tsconfig_file.close();

    const virtual_files = [_]VirtualFile{.{
        .original_path = "src/routes/+page.svelte",
        .virtual_path = "src/routes/+page.svelte.ts",
        .content = "const x = 1;",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    // Test with project tsconfig auto-detection (empty string as workspace path since paths are already relative)
    try writeGeneratedTsconfig(workspace_dir, "", null, &virtual_files);

    // Read generated config
    const content = try workspace_dir.readFileAlloc(allocator, generated_tsconfig, 10 * 1024);

    // Verify extends uses ../ prefix (tsconfig is in .zvelte-check/ subdirectory)
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\": \"../tsconfig.json\"") != null);
}
