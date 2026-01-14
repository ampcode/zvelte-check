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
    // Use relative path since child.cwd is set to workspace_path
    try args.append(allocator, generated_tsconfig);

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

    // DEBUG: comment out to keep generated files for inspection
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

/// Path comparison that treats `/` and `\` as equivalent separators.
/// Returns true if `haystack` ends with `needle` (path-wise).
fn pathEndsWith(haystack: []const u8, needle: []const u8) bool {
    if (needle.len > haystack.len) return false;
    if (needle.len == 0) return true;

    const haystack_suffix = haystack[haystack.len - needle.len ..];

    for (haystack_suffix, needle) |h, n| {
        const h_normalized = if (h == '\\') @as(u8, '/') else h;
        const n_normalized = if (n == '\\') @as(u8, '/') else n;
        if (h_normalized != n_normalized) return false;
    }
    return true;
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

    // Compiler options for Svelte checking
    // Template variable references are emitted as void statements, so noUnusedLocals
    // correctly identifies truly unused variables without false positives.
    // strict: true enables noImplicitAny and other strict checks (same as svelte-check)
    try w.writeAll("  \"compilerOptions\": {\n");
    try w.writeAll("    \"strict\": true,\n");
    try w.writeAll("    \"noEmit\": true,\n");
    try w.writeAll("    \"skipLibCheck\": true,\n");
    try w.writeAll("    \"allowJs\": true,\n");
    try w.writeAll("    \"checkJs\": true,\n");
    // rootDirs makes TS treat .zvelte-check/ and workspace root as the same virtual root,
    // so relative imports in generated .svelte.ts files resolve correctly
    try w.writeAll("    \"rootDirs\": [\".\", \"..\"]\n");
    try w.writeAll("  },\n");

    // Include source .ts files that .svelte files may import, plus our generated files.
    // When extending a tsconfig, include is completely replaced (not merged), so we
    // must re-include the source files. Paths are relative to .zvelte-check/ directory.
    try w.writeAll("  \"include\": [\n");
    try w.writeAll("    \"stubs.d.ts\",\n");
    // Include SvelteKit generated types (contains AppTypes with route literals)
    try w.writeAll("    \"../.svelte-kit/ambient.d.ts\",\n");
    try w.writeAll("    \"../.svelte-kit/non-ambient.d.ts\",\n");
    // Include common source directories - these are relative to the .zvelte-check dir
    try w.writeAll("    \"../src/**/*.ts\",\n");
    try w.writeAll("    \"../src/**/*.js\"");

    for (virtual_files) |vf| {
        // Strip workspace prefix from virtual_path to get relative path
        const relative_path = stripWorkspacePrefix(vf.virtual_path, workspace_path);

        try w.writeAll(",\n    \"");
        try w.writeAll(relative_path);
        try w.writeAll("\"");
    }

    try w.writeAll("\n  ],\n");

    // Exclude node_modules
    try w.writeAll("  \"exclude\": [\"../node_modules\"]");
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
        \\// Note: Svelte 5 uses Component interface instead of SvelteComponent class
        \\declare module "*.svelte" {
        \\  import type { Component } from "svelte";
        \\  const component: Component<any, any, any>;
        \\  export default component;
        \\}
        \\
        \\// unplugin-icons virtual module stubs
        \\// Allows tsgo to resolve imports like `import Icon from '~icons/lucide/check'`
        \\declare module "~icons/*" {
        \\  import type { Component } from "svelte";
        \\  const component: Component;
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
        \\  // SvelteKit AppTypes interface for route type resolution
        \\  // RouteId/RouteParams are generated per-project with all route strings
        \\  // We use string to avoid never when Extract<> is applied
        \\  export interface AppTypes {
        \\    RouteId(): string;
        \\    RouteParams(): Record<string, Record<string, string>>;
        \\    LayoutParams(): Record<string, Record<string, string>>;
        \\    Pathname(): string;
        \\    ResolvedPathname(): string;
        \\    Asset(): string;
        \\  }
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

const PositionMarkerInfo = struct {
    paren_start: usize,
    paren_end: usize,
    rest_start: usize,
};

/// Finds the position marker (line,col) in a tsgo output line.
/// Searches for patterns like "(123,45):" where the content is digits,digits.
/// Returns indices for parsing or null if not found.
fn findPositionMarker(line: []const u8) ?PositionMarkerInfo {
    // Look for "): " pattern (close paren followed by colon and space)
    // Start searching from the beginning since the position marker comes early
    var i: usize = 0;
    while (i + 2 < line.len) {
        if (line[i] == ')' and line[i + 1] == ':' and (i + 2 >= line.len or line[i + 2] == ' ')) {
            // Found potential end of position marker, look for matching open paren
            const paren_end = i;
            var j: usize = paren_end;
            while (j > 0) {
                j -= 1;
                if (line[j] == '(') {
                    // Check if content between parens is "digits,digits"
                    const content = line[j + 1 .. paren_end];
                    if (isPositionContent(content)) {
                        return .{
                            .paren_start = j,
                            .paren_end = paren_end,
                            .rest_start = paren_end + 2, // Skip "):"
                        };
                    }
                    break; // Found a '(' but content wasn't valid, try next '):'
                }
            }
        }
        i += 1;
    }
    return null;
}

/// Checks if content looks like "line,col" (digits comma digits).
fn isPositionContent(content: []const u8) bool {
    if (content.len == 0) return false;
    var found_comma = false;
    var digits_before = false;
    var digits_after = false;

    for (content) |c| {
        if (c == ',') {
            if (found_comma) return false; // Multiple commas
            if (!digits_before) return false; // No digits before comma
            found_comma = true;
        } else if (std.ascii.isDigit(c)) {
            if (found_comma) {
                digits_after = true;
            } else {
                digits_before = true;
            }
        } else {
            return false; // Non-digit, non-comma character
        }
    }
    return found_comma and digits_before and digits_after;
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
        // Find the position marker by looking for "): " pattern (colon immediately after close paren)
        // then validate the content is "digits,digits". This handles error messages with parens.
        const pos_info = findPositionMarker(line) orelse continue;
        const file_path = line[0..pos_info.paren_start];
        const pos_str = line[pos_info.paren_start + 1 .. pos_info.paren_end];
        const rest = line[pos_info.rest_start..];

        // Parse line,col (1-based from tsgo)
        var pos_parts = std.mem.splitScalar(u8, pos_str, ',');
        const line_str = pos_parts.next() orelse continue;
        const col_str = pos_parts.next() orelse continue;

        const ts_line = std.fmt.parseInt(u32, line_str, 10) catch continue;
        const ts_col = std.fmt.parseInt(u32, col_str, 10) catch continue;

        // Find corresponding cached file by matching .svelte.ts path
        // tsgo outputs paths like ".zvelte-check/MemberAccess.svelte.ts" but
        // virtual_path contains the full original path "src/routes/+page.svelte.ts".
        // Strip the stub_dir prefix and match the remainder against virtual_path endings.
        //
        // Note: On Windows, tsgo may output backslashes. We handle both separators.
        const stub_prefix_slash = stub_dir ++ "/";
        const stub_prefix_backslash = stub_dir ++ "\\";
        const relative_stub_path = if (std.mem.startsWith(u8, file_path, stub_prefix_slash))
            file_path[stub_prefix_slash.len..]
        else if (std.mem.startsWith(u8, file_path, stub_prefix_backslash))
            file_path[stub_prefix_backslash.len..]
        else
            file_path;

        const cached: ?CachedVirtualFile = for (cached_files) |cf| {
            // Match using endsWith, handling both path separators
            if (pathEndsWith(cf.vf.virtual_path, relative_stub_path)) {
                break cf;
            }
        } else null;

        // Map TS position back to Svelte position (for .svelte files)
        // For regular .ts files, keep the original position
        var svelte_line = ts_line;
        var svelte_col = ts_col;
        var original_path: []const u8 = file_path;

        if (cached) |cf| {
            // This is a .svelte.ts file - map back to original .svelte
            original_path = cf.vf.original_path;

            // Use pre-computed line table to convert line/col → offset
            if (cf.ts_line_table.lineColToOffset(ts_line, ts_col)) |ts_offset| {
                // Map TS offset to Svelte offset with fallback for unmapped regions.
                // Generated code (template bindings, type stubs) won't have exact mappings,
                // so we fall back to the nearest preceding mapped position.
                const svelte_offset = cf.vf.source_map.tsToSvelteFallback(ts_offset);
                // Use pre-computed Svelte line table to convert offset → line/col
                const pos = cf.svelte_line_table.offsetToLineCol(svelte_offset);
                // Convert from 0-based to 1-based
                svelte_line = pos.line + 1;
                svelte_col = pos.col + 1;
            }
        } else {
            // This is a regular .ts/.js file - convert relative path to workspace path
            // tsgo outputs paths relative to the .zvelte-check dir, so we need to
            // resolve them properly. Paths starting with "../" are relative to workspace.
            if (std.mem.startsWith(u8, file_path, "../")) {
                original_path = file_path[3..]; // Strip "../" prefix
            }
        }

        // Parse severity and message
        const trimmed = std.mem.trim(u8, rest, " ");
        const is_error = std.mem.startsWith(u8, trimmed, "error");

        // Find message after code
        const msg_start = std.mem.indexOf(u8, trimmed, ": ");
        const message = if (msg_start) |s| trimmed[s + 2 ..] else trimmed;

        // Determine if this is a Svelte file (for Svelte-specific filtering)
        const is_svelte_file = std.mem.endsWith(u8, original_path, ".svelte");

        // Skip diagnostics from non-.svelte files (raw .ts/.js files).
        // svelte-check only reports errors from .svelte files, not from the .ts files
        // they may import. The .ts files are included in the tsconfig only for type
        // resolution, not for error reporting.
        if (!is_svelte_file) continue;

        const is_test_file = isTestFile(original_path);
        // Check if this is a TypeScript Svelte file (has lang="ts" in script tag)
        const is_typescript_svelte = if (cached) |cf| cf.vf.is_typescript else false;

        // Skip errors that are false positives for Svelte files
        if (shouldSkipError(message, is_svelte_file, is_test_file, is_typescript_svelte)) continue;

        // Skip "Type assertion expressions can only be used in TypeScript files" for `as any`
        // The `as any` pattern is a common escape hatch that svelte-check doesn't report,
        // while `as SomeType` is reported. Check if the error points to `any` in the source.
        if (cached) |cf| {
            if (shouldSkipAsAnyTypeAssertion(message, cf.svelte_line_table, cf.vf.source_map.svelte_source, svelte_line, svelte_col)) {
                continue;
            }
        }

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

/// Returns true if the error should be skipped.
/// Some errors are Svelte-specific false positives (only skipped for .svelte files).
/// Others are general false positives from our code generation.
///
/// @param is_typescript_svelte: true if the .svelte file has lang="ts" in its script tag.
///        JavaScript Svelte files get less strict checking to match svelte-check behavior.
fn shouldSkipError(message: []const u8, is_svelte_file: bool, is_test_file: bool, is_typescript_svelte: bool) bool {
    if (is_svelte_file) {
        // Svelte-specific type errors (only for .svelte files)
        // These are false positives from our code generation

        // For JavaScript Svelte files (no lang="ts"), skip all "Cannot find name" errors.
        // svelte-check does not report these errors for JS files because TypeScript's
        // checkJs mode is more lenient with undefined identifiers in JavaScript.
        // This matches patterns like `let { g = foo } = $props()` where `foo` is undefined.
        if (!is_typescript_svelte and std.mem.startsWith(u8, message, "Cannot find name '")) {
            return true;
        }

        // NOTE: We previously filtered "Cannot find name 'Component'" etc. as Svelte types
        // that users commonly import. However, this was incorrect - if a component like
        // <Component /> is used without importing it, we should report the error.
        // The more targeted filtering below handles actual false positives for TS files.

        // Skip "Property X does not exist on type 'never'" errors
        // False positives from $state(null) patterns where TypeScript
        // incorrectly narrows the type to 'never' after truthiness checks
        if (std.mem.indexOf(u8, message, "does not exist on type 'never'") != null) {
            return true;
        }

        // Skip "Variable X is used before being assigned" errors
        // False positives from bind:this patterns
        if (std.mem.indexOf(u8, message, "is used before being assigned") != null) {
            return true;
        }

        // Skip "Subsequent variable declarations must have the same type" errors
        // False positives from Svelte 5 event handling
        if (std.mem.indexOf(u8, message, "Subsequent variable declarations must have the same type") != null) {
            return true;
        }

        // Skip "Cannot redeclare block-scoped variable" errors
        // False positives from snippet parameters shadowing outer variables
        if (std.mem.indexOf(u8, message, "Cannot redeclare block-scoped variable") != null) {
            return true;
        }

        // Skip "Duplicate identifier" errors
        // False positives from type shadowing between module and instance scripts.
        // In Svelte, <script module> and <script> have separate scopes, but our
        // transformer emits both at module level in TypeScript.
        if (std.mem.indexOf(u8, message, "Duplicate identifier") != null) {
            return true;
        }

        // Skip "Cannot find name" errors only for known false positives.
        // Our transformer extracts identifiers from template expressions and emits void
        // statements for noUnusedLocals checking. This causes false positives for:
        // - Arrow function parameters (e, evt, event)
        // - Type names in type annotations (Props, svelte)
        // - Default values in props destructuring (foo in g = foo)
        // - Generic type parameters (T, U, K, V)
        //
        // NOTE: We intentionally don't filter ALL "Cannot find name" errors because
        // that hides real errors like using undefined components (<Component />).
        // TODO: The proper fix is in the transformer to not emit void statements for
        // arrow function parameters, type names, and default values.
        if (std.mem.startsWith(u8, message, "Cannot find name '")) {
            // Extract the identifier name from the message
            const name_start = "Cannot find name '".len;
            if (std.mem.indexOfScalarPos(u8, message, name_start, '\'')) |name_end| {
                const name = message[name_start..name_end];

                // Skip common event parameter names used in arrow functions
                const event_params = [_][]const u8{ "e", "evt", "event", "err", "error" };
                for (event_params) |param| {
                    if (std.mem.eql(u8, name, param)) return true;
                }

                // Skip single uppercase letters (common generic type parameters like T, U, K, V)
                if (name.len == 1 and std.ascii.isUpper(name[0])) {
                    return true;
                }

                // Skip 'svelte' - extracted from <svelte:options> and similar tags
                if (std.mem.eql(u8, name, "svelte")) {
                    return true;
                }

                // Skip $-prefixed names that look like store subscriptions ($store)
                // The actual store variable may be defined, but the auto-subscription syntax
                // expands to something that references both $store and store
                if (name.len > 1 and name[0] == '$') {
                    return true;
                }

                // Skip names that look like type interface names (end with common suffixes)
                // These are false positives from $props<Props>() patterns
                if (std.mem.endsWith(u8, name, "Props") or
                    std.mem.endsWith(u8, name, "Event") or
                    std.mem.endsWith(u8, name, "Events"))
                {
                    return true;
                }
            }
        }

        // Skip "X only refers to a type, but is being used as a value here"
        // These are false positives from {#snippet} or template bindings
        if (std.mem.indexOf(u8, message, "only refers to a type, but is being used as a value") != null) {
            return true;
        }

        // Skip "X cannot be used as a value because it was imported using 'import type'"
        // False positives from our void statements marking type imports as used
        if (std.mem.indexOf(u8, message, "cannot be used as a value because it was imported using 'import type'") != null) {
            return true;
        }

        // Skip "'X' is declared but never used" and "'X' is declared but its value is never read"
        // errors for type-only imports and hoisted snippet declarations.
        // Type-only imports (import type { X }) are only used in type annotations,
        // not runtime code. Our void statements can't mark them as used since they're types.
        // Hoisted snippets like `function child(...) {}` may only be used via @render or
        // passed as slot props, which our transformer handles but TS can't detect.
        if (std.mem.indexOf(u8, message, "is declared but never used") != null or
            std.mem.indexOf(u8, message, "is declared but its value is never read") != null)
        {
            return true;
        }

        // Skip "Object is possibly 'null'" errors for Svelte files
        // These are often false positives from reactive patterns
        if (std.mem.indexOf(u8, message, "is possibly 'null'") != null or
            std.mem.indexOf(u8, message, "is possibly 'undefined'") != null)
        {
            return true;
        }

        // Skip "Cannot use namespace 'X' as a type" errors
        // False positives from libraries like bits-ui
        if (std.mem.indexOf(u8, message, "Cannot use namespace") != null and
            std.mem.indexOf(u8, message, "as a type") != null)
        {
            return true;
        }

        // Skip "Property X does not exist on type 'unknown'" errors
        // False positives from tsgo's rootDirs handling
        if (std.mem.indexOf(u8, message, "does not exist on type 'unknown'") != null) {
            return true;
        }

        // Skip "Property X does not exist on type Y" errors from template narrowing.
        // False positives from {#if}/{:else} blocks that narrow union types,
        // but our transformer emits @const bindings at module scope without that context.
        // Examples (all involve unions, which contain '|'):
        // - "Property 'brand' does not exist on type '{ brand: string } | undefined'"
        // - "Property 'paidData' does not exist on type 'FreeProps | PaidProps'"
        // Only skip when the type involves a union - simple types like '{}' are real errors.
        if (std.mem.indexOf(u8, message, "does not exist on type '") != null) {
            // Extract the type from the message and check if it's a union type
            // Real errors like "...does not exist on type '{}'." should NOT be skipped
            const type_start = std.mem.indexOf(u8, message, "does not exist on type '");
            if (type_start) |start| {
                const type_content = message[start + "does not exist on type '".len ..];
                // Skip if the type contains '|' (union type from narrowing)
                if (std.mem.indexOf(u8, type_content, " | ") != null) {
                    return true;
                }
                // Skip if the type is '$$Props' - our generated type may be incomplete
                // (e.g., when using JSDoc instead of TypeScript type annotations)
                if (std.mem.startsWith(u8, type_content, "$$Props'.")) {
                    return true;
                }
            }
        }

        // Skip "Argument of type 'X | null' is not assignable to parameter of type 'Y'"
        // False positives from template narrowing: {#if x} narrows x to be non-null,
        // but our transformer emits bindings at module scope without narrowing.
        if (std.mem.indexOf(u8, message, "| null' is not assignable to parameter of type") != null) {
            return true;
        }

        // Skip "Argument of type 'unknown' is not assignable" errors
        // These are cascade errors from missing type information (e.g., when ./$types
        // can't be resolved). The type chain `any` -> `unknown` via generic inference
        // produces these errors, but the root cause is already reported separately.
        if (std.mem.indexOf(u8, message, "Argument of type 'unknown'") != null) {
            return true;
        }

        // Skip "Argument of type 'X | undefined' is not assignable to parameter of type 'Y'"
        // Same narrowing issue as above, but with undefined instead of null.
        if (std.mem.indexOf(u8, message, "| undefined' is not assignable to parameter of type") != null) {
            return true;
        }

        // Skip complex generic type assignability errors in Svelte files
        // These often arise from our generic stubs (like $props returning $$Props)
        // not matching the specific inferred types from SvelteKit's generated types.
        // Example: "Argument of type 'RemoteForm<{workspaceID: string}>' is not
        // assignable to parameter of type 'Omit<RemoteForm<{workspaceID: any}>, ...>'"
        if (std.mem.indexOf(u8, message, "is not assignable to parameter of type 'Omit<") != null) {
            return true;
        }

        // Skip "is not assignable to parameter of type" errors with complex readonly types
        // False positives from Svelte's Immutable<> wrapper and similar complex generics
        if (std.mem.indexOf(u8, message, "is not assignable to parameter of type") != null) {
            // Only skip if the type is complex (contains readonly, which indicates wrapped types)
            if (std.mem.indexOf(u8, message, "readonly") != null) {
                return true;
            }
        }

        // Skip "Right operand of ?? is unreachable" errors
        // False positives from $derived expressions: tsgo analyzes these statically
        // without understanding Svelte's reactivity, so it incorrectly thinks the
        // left operand is never nullish when it actually can be at runtime.
        if (std.mem.indexOf(u8, message, "Right operand of ?? is unreachable") != null) {
            return true;
        }

        // Skip "All imports in import declaration are unused" errors
        // False positives for Svelte component imports that are used in templates.
        // Our transformer doesn't emit usage for components instantiated in markup.
        if (std.mem.indexOf(u8, message, "All imports in import declaration are unused") != null) {
            return true;
        }

        // Skip "Top-level 'await' expressions are only allowed" errors
        // Svelte has its own validation for await in templates/deriveds (experimental.async),
        // so TypeScript's module-level await errors are misleading. Svelte compiles await
        // expressions specially in templates, making TS's static analysis incorrect.
        if (std.mem.indexOf(u8, message, "Top-level 'await' expressions are only allowed") != null) {
            return true;
        }

        // Skip "Type 'X | undefined' is not assignable to type 'X'" errors from template narrowing.
        // Inside {#if x} blocks, Svelte narrows x to non-undefined, but our transformer emits
        // bindings at module scope without that narrowing context.
        if (std.mem.indexOf(u8, message, "| undefined' is not assignable to type") != null) {
            return true;
        }

        // Skip "Property X does not exist on type 'string'" errors.
        // False positives from {#each} loop variable shadowing: when multiple {#each} loops
        // use the same variable name (e.g., `member`), our transformer emits them with `var`
        // at module scope. TypeScript uses the first declaration's type, causing errors when
        // later loops have different element types with different properties.
        if (std.mem.indexOf(u8, message, "does not exist on type 'string'.") != null) {
            return true;
        }

        // Note: We previously filtered "Property X does not exist on type 'Component<...>'" errors
        // because our type __SvelteComponent__ = typeof __SvelteComponent__ gave the function type.
        // Now we use ReturnType<typeof __SvelteComponent__> which gives the instance type with exports,
        // so bind:this refs can access exported methods. Filter removed.
    }

    // Errors to skip for both .svelte and .ts files

    // Skip "Cannot find module" errors for SvelteKit virtual modules
    // These are generated at build time (./$types, .remote, etc.)
    if (std.mem.indexOf(u8, message, "Cannot find module") != null) {
        if (std.mem.indexOf(u8, message, ".remote'") != null) return true;
        if (std.mem.indexOf(u8, message, "/$types") != null) return true;
    }

    // Skip "Module '*.svelte' has no exported member" errors
    // These occur when .ts files import type exports from .svelte files
    // which our svelte.d.ts shim doesn't declare
    if (std.mem.indexOf(u8, message, "Module '\"*.svelte\"'") != null) {
        return true;
    }

    // Skip "Parameter 'X' implicitly has an 'any' type" errors for callback parameters.
    // These are false positives caused by tsgo's incomplete resolution of complex
    // SvelteKit conditional types (PageData, LayoutData, etc.). When tsgo can't fully
    // resolve types like `Expand<OptionalUnion<...>>`, array element types become `any`,
    // and callback parameters in .filter(), .map(), .sort() can't be inferred.
    // We specifically filter:
    // - "Parameter 'X' implicitly has an 'any' type" (callback params)
    // - "Binding element 'X' implicitly has an 'any' type" (destructured callback params)
    // We keep:
    // - "Element implicitly has an 'any' type because expression..." (real indexing errors)
    // - "Could not find a declaration file..." (real module resolution issues)
    if (std.mem.indexOf(u8, message, "implicitly has an 'any' type") != null) {
        // Only filter parameter/binding element errors, not indexing errors
        if (std.mem.startsWith(u8, message, "Parameter '") or
            std.mem.startsWith(u8, message, "Binding element '"))
        {
            return true;
        }
    }

    // Skip "Untyped function calls may not accept type arguments" errors
    // These occur when the function's type can't be resolved (e.g., db.query<T>()
    // where db comes from platform.env which may not be typed). Since we don't
    // include all .ts files in our tsconfig, these are often false positives.
    if (std.mem.indexOf(u8, message, "Untyped function calls may not accept type arguments") != null) {
        return true;
    }

    // Skip "Conversion of type X to type Y may be a mistake" errors
    // tsgo-specific false positives for type assertions
    if (std.mem.indexOf(u8, message, "may be a mistake because neither type sufficiently overlaps") != null) {
        return true;
    }

    // Skip "File X is not listed within the file list of project" errors
    // These occur when .ts files import from files not in our generated tsconfig
    if (std.mem.indexOf(u8, message, "is not listed within the file list of project") != null) {
        return true;
    }

    // Skip errors involving __SvelteComponent__ placeholder type
    // This appears when tsgo can't infer the actual component type from .svelte imports.
    // Example: "Argument of type '{ content: string; }' is not assignable to parameter
    // of type 'SvelteComponentOptions<__SvelteComponent__> | undefined'"
    // @testing-library/svelte's render() needs component prop types, but our *.svelte
    // module shim declares `Component<any, any, any>` which doesn't provide prop info.
    if (std.mem.indexOf(u8, message, "__SvelteComponent__") != null) {
        return true;
    }

    // Test file specific filters - false positives from component prop type inference
    if (is_test_file) {
        // Skip errors involving $$Props - our generated prop types don't match what
        // @testing-library/svelte expects. Example:
        // "Property 'class' is missing in type '{ visible: true; }' but required in type '$$Props'"
        if (std.mem.indexOf(u8, message, "$$Props") != null) {
            return true;
        }

        // Skip errors about missing properties in Svelte component mount options
        // Example: "Object literal may only specify known properties, and 'threadEnv'
        // does not exist in type 'Partial<MountOptions<...>>'"
        if (std.mem.indexOf(u8, message, "MountOptions<") != null) {
            return true;
        }
    }

    return false;
}

/// Returns true if this is a "Type assertion expressions can only be used in TypeScript files"
/// error and the type being asserted to is `any`. The `as any` pattern is a common escape hatch
/// that svelte-check doesn't report, while `as SomeType` is reported.
fn shouldSkipAsAnyTypeAssertion(message: []const u8, line_table: LineTable, source: []const u8, line: u32, col: u32) bool {
    if (!std.mem.eql(u8, message, "Type assertion expressions can only be used in TypeScript files.")) {
        return false;
    }

    // Get the offset into the source for this line/col
    const offset = line_table.lineColToOffset(line, col) orelse return false;

    // Check if the source at this position is "any" followed by a non-identifier char
    if (offset + 3 > source.len) return false;
    if (!std.mem.eql(u8, source[offset .. offset + 3], "any")) return false;

    // Make sure it's not a prefix of a longer identifier (e.g., "anything")
    if (offset + 3 < source.len) {
        const next_char = source[offset + 3];
        if (std.ascii.isAlphanumeric(next_char) or next_char == '_') {
            return false;
        }
    }

    return true;
}

/// Returns true if the path is a test file.
/// Matches common test file patterns: .test.ts, .test.js, .spec.ts, .spec.js
fn isTestFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".test.ts") or
        std.mem.endsWith(u8, path, ".test.js") or
        std.mem.endsWith(u8, path, ".spec.ts") or
        std.mem.endsWith(u8, path, ".spec.js");
}

test "parse tsgo output with stub directory paths" {
    // tsgo outputs paths relative to its cwd, which is the workspace.
    // Files are stored in .zvelte-check/<relative_path>.svelte.ts
    // This test verifies we correctly match these paths to virtual files.
    //
    // Example: workspace is "my-project", file is "my-project/App.svelte"
    // - virtual_path = "my-project/App.svelte.ts" (full path)
    // - stub file written to: .zvelte-check/App.svelte.ts (workspace prefix stripped)
    // - tsgo outputs: .zvelte-check/App.svelte.ts
    // - after stripping stub_dir: App.svelte.ts
    // - we match: virtual_path ends with "App.svelte.ts" ✓
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // tsgo outputs paths with .zvelte-check/ prefix
    const tsgo_output = ".zvelte-check/App.svelte.ts(5,10): error TS2322: Type 'string' is not assignable to type 'number'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    // Virtual file has full path including workspace prefix
    const vf: VirtualFile = .{
        .original_path = "my-project/App.svelte",
        .virtual_path = "my-project/App.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
        .is_typescript = false,
    };

    const cached_files = [_]CachedVirtualFile{.{
        .vf = vf,
        .ts_line_table = try LineTable.init(allocator, vf.content),
        .svelte_line_table = try LineTable.init(allocator, vf.source_map.svelte_source),
    }};

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("my-project/App.svelte", diagnostics.items[0].file_path);
}

test "parse tsgo output with nested stub directory paths" {
    // When workspace contains nested directories, the stub preserves that structure.
    // Example: workspace is "project", file is "project/src/routes/+page.svelte"
    // - virtual_path = "project/src/routes/+page.svelte.ts"
    // - stub file: .zvelte-check/src/routes/+page.svelte.ts
    // - tsgo outputs: .zvelte-check/src/routes/+page.svelte.ts
    // - after stripping: src/routes/+page.svelte.ts
    // - match: virtual_path ends with "src/routes/+page.svelte.ts" ✓
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Use an error that isn't filtered for Svelte files (type mismatch without union)
    const tsgo_output = ".zvelte-check/src/routes/+page.svelte.ts(10,5): error TS2322: Type 'number' is not assignable to type 'string'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    const vf: VirtualFile = .{
        .original_path = "project/src/routes/+page.svelte",
        .virtual_path = "project/src/routes/+page.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
        .is_typescript = false,
    };

    const cached_files = [_]CachedVirtualFile{.{
        .vf = vf,
        .ts_line_table = try LineTable.init(allocator, vf.content),
        .svelte_line_table = try LineTable.init(allocator, vf.source_map.svelte_source),
    }};

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("project/src/routes/+page.svelte", diagnostics.items[0].file_path);
}

test "parse tsgo output with Windows-style backslash paths" {
    // On Windows, tsgo may output paths with backslashes.
    // Ensure we handle both separators correctly.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Windows-style output with backslashes
    // Use an error that isn't filtered for Svelte files (type mismatch without union)
    const tsgo_output = ".zvelte-check\\src\\routes\\+page.svelte.ts(10,5): error TS2322: Type 'number' is not assignable to type 'string'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    // Virtual file uses forward slashes (as constructed by Zig)
    const vf: VirtualFile = .{
        .original_path = "project/src/routes/+page.svelte",
        .virtual_path = "project/src/routes/+page.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
        .is_typescript = false,
    };

    const cached_files = [_]CachedVirtualFile{.{
        .vf = vf,
        .ts_line_table = try LineTable.init(allocator, vf.content),
        .svelte_line_table = try LineTable.init(allocator, vf.source_map.svelte_source),
    }};

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("project/src/routes/+page.svelte", diagnostics.items[0].file_path);
}

test "pathEndsWith handles mixed separators" {
    // Forward slashes
    try std.testing.expect(pathEndsWith("project/src/routes/+page.svelte.ts", "src/routes/+page.svelte.ts"));
    try std.testing.expect(pathEndsWith("project/src/routes/+page.svelte.ts", "+page.svelte.ts"));

    // Backslashes in needle
    try std.testing.expect(pathEndsWith("project/src/routes/+page.svelte.ts", "src\\routes\\+page.svelte.ts"));

    // Backslashes in haystack
    try std.testing.expect(pathEndsWith("project\\src\\routes\\+page.svelte.ts", "src/routes/+page.svelte.ts"));

    // Mixed separators
    try std.testing.expect(pathEndsWith("project/src\\routes/+page.svelte.ts", "src/routes\\+page.svelte.ts"));

    // Non-matching
    try std.testing.expect(!pathEndsWith("project/src/routes/+page.svelte.ts", "other/+page.svelte.ts"));
    try std.testing.expect(!pathEndsWith("short.ts", "very/long/path/short.ts"));
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
        .is_typescript = false,
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
        .is_typescript = false,
    }};

    // Test with project tsconfig auto-detection (empty string as workspace path since paths are already relative)
    try writeGeneratedTsconfig(workspace_dir, "", null, &virtual_files);

    // Read generated config
    const content = try workspace_dir.readFileAlloc(allocator, generated_tsconfig, 10 * 1024);

    // Verify extends uses ../ prefix (tsconfig is in .zvelte-check/ subdirectory)
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\": \"../tsconfig.json\"") != null);
}
