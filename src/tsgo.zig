//! tsgo runner - spawns tsgo for TypeScript type-checking
//!
//! Writes transformed .svelte.ts files alongside their .svelte sources,
//! generates a tsconfig that extends the project's config, runs tsgo
//! from the workspace root for proper module resolution, then cleans up.

const std = @import("std");
const VirtualFile = @import("transformer.zig").VirtualFile;
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const SourceMap = @import("source_map.zig").SourceMap;
const LineTable = @import("source_map.zig").LineTable;

const generated_tsconfig = ".svelte-check-tsconfig.json";
const generated_stubs = ".svelte-check-stubs.d.ts";

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

    // Track generated files for cleanup
    var generated_files: std.ArrayList([]const u8) = .empty;
    defer {
        for (generated_files.items) |path| {
            allocator.free(path);
        }
        generated_files.deinit(allocator);
    }
    errdefer cleanupGeneratedFiles(workspace_dir, generated_files.items);

    // Write transformed .svelte.ts files alongside .svelte sources
    for (virtual_files) |vf| {
        // Strip workspace prefix from virtual_path to get relative path
        const relative_path = stripWorkspacePrefix(vf.virtual_path, workspace_path);

        try writeVirtualFile(workspace_dir, relative_path, vf.content);
        try generated_files.append(allocator, try allocator.dupe(u8, relative_path));
    }

    // Write SvelteKit ambient type stubs
    try writeSvelteKitStubs(workspace_dir);
    try generated_files.append(allocator, try allocator.dupe(u8, generated_stubs));

    // Generate tsconfig that extends project config and includes only our files
    try writeGeneratedTsconfig(workspace_dir, workspace_path, tsconfig_path, virtual_files);
    try generated_files.append(allocator, try allocator.dupe(u8, generated_tsconfig));

    // Build tsgo command
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    try args.append(allocator, "tsgo");
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

    try child.spawn();

    // Collect stdout/stderr concurrently to avoid pipe deadlock
    var stdout_buf: std.ArrayList(u8) = .empty;
    defer stdout_buf.deinit(allocator);
    var stderr_buf: std.ArrayList(u8) = .empty;
    defer stderr_buf.deinit(allocator);

    try child.collectOutput(allocator, &stdout_buf, &stderr_buf, 10 * 1024 * 1024);
    const result = try child.wait();

    const stdout = stdout_buf.items;
    const stderr = stderr_buf.items;

    // Clean up generated files before parsing output
    cleanupGeneratedFiles(workspace_dir, generated_files.items);

    // Parse tsgo output
    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    const has_output = stdout.len > 0 or stderr.len > 0;
    const exited_with_error = switch (result) {
        .Exited => |code| code != 0,
        else => true,
    };

    if (exited_with_error or has_output) {
        try parseTsgoOutput(allocator, stdout, virtual_files, &diagnostics);
        if (stderr.len > 0) {
            try parseTsgoOutput(allocator, stderr, virtual_files, &diagnostics);
        }
    }

    return diagnostics.toOwnedSlice(allocator);
}

/// Writes a transformed .svelte.ts file alongside its .svelte source.
/// `relative_path` should be relative to workspace_dir (not include workspace prefix).
fn writeVirtualFile(workspace_dir: std.fs.Dir, relative_path: []const u8, content: []const u8) !void {
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

/// Cleans up generated .svelte.ts files and tsconfig.
fn cleanupGeneratedFiles(workspace_dir: std.fs.Dir, paths: []const []const u8) void {
    for (paths) |path| {
        workspace_dir.deleteFile(path) catch {};
    }
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
    if (tsconfig_path) |ptsconfig| {
        try w.writeAll("  \"extends\": \"./");
        try w.writeAll(ptsconfig);
        try w.writeAll("\",\n");
    } else {
        // Auto-detect tsconfig.json in workspace root
        if (workspace_dir.access("tsconfig.json", .{})) {
            try w.writeAll("  \"extends\": \"./tsconfig.json\",\n");
        } else |_| {}
    }

    // Compiler options optimized for Svelte checking
    try w.writeAll("  \"compilerOptions\": {\n");
    try w.writeAll("    \"noEmit\": true,\n");
    try w.writeAll("    \"skipLibCheck\": true,\n");
    try w.writeAll("    \"allowJs\": true,\n");
    try w.writeAll("    \"checkJs\": true,\n");
    try w.writeAll("    \"noUnusedLocals\": false,\n");
    try w.writeAll("    \"noUnusedParameters\": false\n");
    try w.writeAll("  },\n");

    // Include our generated files plus all .ts files they may import
    try w.writeAll("  \"include\": [\n");
    try w.writeAll("    \"");
    try w.writeAll(generated_stubs);
    try w.writeAll("\",\n");
    try w.writeAll("    \"src/**/*.ts\"");

    for (virtual_files) |vf| {
        // Strip workspace prefix from virtual_path to get relative path
        const relative_path = stripWorkspacePrefix(vf.virtual_path, workspace_path);

        try w.writeAll(",\n    \"");
        try w.writeAll(relative_path);
        try w.writeAll("\"");
    }

    try w.writeAll("\n  ],\n");

    // Exclude node_modules to avoid scanning everything
    try w.writeAll("  \"exclude\": [\"node_modules\"]\n");
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
        \\// Auto-generated by svelte-check-zig
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
    virtual_files: []const VirtualFile,
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

        // Find corresponding virtual file by matching .svelte.ts path
        const vf: ?VirtualFile = for (virtual_files) |v| {
            if (std.mem.endsWith(u8, file_path, v.virtual_path)) {
                break v;
            }
        } else null;

        // Map TS position back to Svelte position
        var svelte_line = ts_line;
        var svelte_col = ts_col;
        var original_path: []const u8 = file_path;

        if (vf) |v| {
            original_path = v.original_path;

            // Build line table for generated TS to convert line/col → offset
            const ts_table = try LineTable.init(allocator, v.content);

            if (ts_table.lineColToOffset(ts_line, ts_col)) |ts_offset| {
                // Map TS offset to Svelte offset
                if (v.source_map.tsToSvelte(ts_offset)) |svelte_offset| {
                    // Build line table for Svelte source to convert offset → line/col
                    const svelte_table = try LineTable.init(allocator, v.source_map.svelte_source);
                    const pos = svelte_table.offsetToLineCol(svelte_offset);
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

test "parse tsgo output" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tsgo_output = "src/routes/+page.svelte.ts(5,10): error TS2322: Type 'string' is not assignable to type 'number'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    const virtual_files = [_]VirtualFile{.{
        .original_path = "src/routes/+page.svelte",
        .virtual_path = "src/routes/+page.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    try parseTsgoOutput(allocator, tsgo_output, &virtual_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("src/routes/+page.svelte", diagnostics.items[0].file_path);
}

test "writeGeneratedTsconfig generates valid config" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp workspace dir to test
    var workspace_dir = try std.fs.cwd().makeOpenPath(".svelte-check-zig-test-workspace", .{});
    defer {
        workspace_dir.close();
        std.fs.cwd().deleteTree(".svelte-check-zig-test-workspace") catch {};
    }

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
    // Verify .ts files are included for imports from .svelte files
    try std.testing.expect(std.mem.indexOf(u8, content, "\"src/**/*.ts\"") != null);
}

test "writeGeneratedTsconfig extends project config when available" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp workspace dir to test
    var workspace_dir = try std.fs.cwd().makeOpenPath(".svelte-check-zig-test-workspace", .{});
    defer {
        workspace_dir.close();
        std.fs.cwd().deleteTree(".svelte-check-zig-test-workspace") catch {};
    }

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

    // Verify extends is present
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\": \"./tsconfig.json\"") != null);
}
