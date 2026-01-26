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
const sveltekit_routes = @import("sveltekit_routes.zig");

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

/// Represents a transformed SvelteKit route file (.ts)
const TransformedRouteFile = struct {
    original_path: []const u8, // Original .ts file path
    transformed_path: []const u8, // Path where transformed content is written
    content: []const u8, // Transformed content
};

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

    // Create stub directory for tsconfig and stubs.d.ts
    try workspace_dir.makePath(stub_dir);
    errdefer cleanupStubDir(workspace_dir);

    // Track written files for cleanup (includes files outside workspace from referenced projects)
    var written_files: std.ArrayList([]const u8) = .empty;
    defer {
        for (written_files.items) |path| allocator.free(path);
        written_files.deinit(allocator);
    }
    errdefer cleanupWrittenFiles(written_files.items);

    // Track transformed route files
    var transformed_routes: std.ArrayList(TransformedRouteFile) = .empty;
    defer {
        for (transformed_routes.items) |tf| {
            allocator.free(tf.content);
            allocator.free(tf.transformed_path);
            allocator.free(tf.original_path);
        }
        transformed_routes.deinit(allocator);
    }

    // Write transformed .svelte.ts files alongside original .svelte files.
    // This allows TypeScript to find them via normal module resolution, even for
    // imports from referenced monorepo packages.
    for (virtual_files) |vf| {
        // virtual_path is already the absolute path to the .svelte.ts file
        const ts_path = try allocator.dupe(u8, vf.virtual_path);
        try written_files.append(allocator, ts_path);

        // Write file using absolute path
        const file = try std.fs.cwd().createFile(ts_path, .{});
        defer file.close();
        try file.writeAll(vf.content);
    }

    // Find and transform SvelteKit route .ts files
    try findAndTransformRouteFiles(allocator, workspace_dir, workspace_path, &transformed_routes, &written_files);

    // Write SvelteKit ambient type stubs
    try writeSvelteKitStubs(workspace_dir);

    // Generate tsconfig that extends project config and includes only our files
    try writeGeneratedTsconfig(workspace_dir, tsconfig_path, virtual_files, transformed_routes.items);

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

    // Restore original route files that were temporarily transformed
    for (transformed_routes.items) |tf| {
        // tf.content now contains the ORIGINAL content that we saved before transformation
        const file = std.fs.cwd().createFile(tf.original_path, .{}) catch continue;
        defer file.close();
        file.writeAll(tf.content) catch {};
    }

    // Clean up generated .svelte.ts files
    cleanupWrittenFiles(written_files.items);
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

        try parseTsgoOutput(allocator, stdout, cached_files.items, workspace_path, &diagnostics);
        if (stderr.len > 0) {
            try parseTsgoOutput(allocator, stderr, cached_files.items, workspace_path, &diagnostics);
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

/// Extracts the workspace path from a stub directory path.
/// Stub path is like /path/to/workspace/.zvelte-check/src/routes/+page.server.ts
/// Returns /path/to/workspace
fn getWorkspaceFromStubDir(stub_path: []const u8) []const u8 {
    if (std.mem.indexOf(u8, stub_path, stub_dir)) |idx| {
        // Return everything before /.zvelte-check
        if (idx > 0) return stub_path[0 .. idx - 1];
    }
    return stub_path;
}

/// Cleans up the stub directory containing tsconfig and stubs.d.ts.
fn cleanupStubDir(workspace_dir: std.fs.Dir) void {
    workspace_dir.deleteTree(stub_dir) catch {};
}

/// Cleans up written .svelte.ts files that were placed alongside .svelte files.
fn cleanupWrittenFiles(paths: []const []const u8) void {
    for (paths) |path| {
        std.fs.cwd().deleteFile(path) catch {};
    }
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
    tsconfig_path: ?[]const u8,
    virtual_files: []const VirtualFile,
    transformed_routes: []const TransformedRouteFile,
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
    try w.writeAll("    \"checkJs\": true\n");
    try w.writeAll("  },\n");

    // Include source .ts files that .svelte files may import, plus our generated files.
    // When extending a tsconfig, include is completely replaced (not merged), so we
    // must re-include the source files. Paths are relative to .zvelte-check/ directory.
    // Generated .svelte.ts files are now written in-place alongside original .svelte files,
    // which may be in referenced projects outside the workspace.
    try w.writeAll("  \"include\": [\n");
    try w.writeAll("    \"stubs.d.ts\",\n");
    // Include SvelteKit generated type declarations for $app/types and route types.
    // The parent tsconfig's include is overridden by ours, so we must explicitly include
    // these files to resolve AppTypes.RouteId for route type-checking.
    try w.writeAll("    \"../.svelte-kit/ambient.d.ts\",\n");
    try w.writeAll("    \"../.svelte-kit/non-ambient.d.ts\",\n");
    try w.writeAll("    \"../.svelte-kit/types/**/$types.d.ts\",\n");
    // Include common source directories - these are relative to the .zvelte-check dir
    try w.writeAll("    \"../src/**/*.ts\",\n");
    try w.writeAll("    \"../src/**/*.js\"");

    for (virtual_files) |vf| {
        // Use absolute path since files may be outside workspace (from referenced projects)
        try w.writeAll(",\n    \"");
        try w.writeAll(vf.virtual_path);
        try w.writeAll("\"");
    }

    // Include transformed route files
    for (transformed_routes) |tf| {
        try w.writeAll(",\n    \"");
        try w.writeAll(tf.transformed_path);
        try w.writeAll("\"");
    }

    try w.writeAll("\n  ],\n");

    // Exclude node_modules
    // Note: we don't need to exclude transformed route files anymore since we overwrite them in-place
    try w.writeAll("  \"exclude\": [\"../node_modules\"]\n");
    try w.writeAll("}\n");

    try w.flush();
}

/// Finds and transforms SvelteKit route .ts files in the workspace.
/// Transformed files are written to the stub directory and tracked for cleanup.
fn findAndTransformRouteFiles(
    allocator: std.mem.Allocator,
    workspace_dir: std.fs.Dir,
    workspace_path: []const u8,
    transformed_routes: *std.ArrayList(TransformedRouteFile),
    written_files: *std.ArrayList([]const u8),
) !void {
    // Walk src/routes directory looking for SvelteKit route files
    const src_routes_path = "src/routes";
    var routes_dir = workspace_dir.openDir(src_routes_path, .{ .iterate = true }) catch return;
    defer routes_dir.close();

    try walkRouteDirectory(allocator, routes_dir, workspace_dir, workspace_path, src_routes_path, transformed_routes, written_files);
}

/// Recursively walks a directory looking for SvelteKit route .ts files
fn walkRouteDirectory(
    allocator: std.mem.Allocator,
    dir: std.fs.Dir,
    workspace_dir: std.fs.Dir,
    workspace_path: []const u8,
    current_path: []const u8,
    transformed_routes: *std.ArrayList(TransformedRouteFile),
    written_files: *std.ArrayList([]const u8),
) !void {
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        switch (entry.kind) {
            .directory => {
                // Skip node_modules etc.
                if (std.mem.eql(u8, entry.name, "node_modules")) continue;
                if (std.mem.eql(u8, entry.name, ".git")) continue;

                const subdir_path = try std.fs.path.join(allocator, &.{ current_path, entry.name });
                defer allocator.free(subdir_path);

                var subdir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer subdir.close();

                try walkRouteDirectory(allocator, subdir, workspace_dir, workspace_path, subdir_path, transformed_routes, written_files);
            },
            .file => {
                // Check if this is a SvelteKit route file that needs transformation
                const kind = sveltekit_routes.getRouteFileKind(entry.name);
                if (kind == .none) continue;

                // Read the file
                const file_path = try std.fs.path.join(allocator, &.{ current_path, entry.name });
                defer allocator.free(file_path);

                const source = workspace_dir.readFileAlloc(allocator, file_path, 10 * 1024 * 1024) catch |err| {
                    std.log.debug("Failed to read route file {s}: {}", .{ file_path, err });
                    continue;
                };

                // Full path for diagnostics (absolute path)
                const full_path = try std.fs.path.join(allocator, &.{ workspace_path, file_path });
                defer allocator.free(full_path);

                // Transform the file
                const transformed = try sveltekit_routes.transformRouteFile(allocator, source, entry.name);
                if (transformed) |content| {
                    defer allocator.free(content); // Free the transformed content after writing

                    // Write transformed file IN-PLACE (overwrite original temporarily)
                    // This ensures TypeScript sees the transformed version since we include ../src/**/*.ts
                    const file = try workspace_dir.createFile(file_path, .{});
                    defer file.close();
                    try file.writeAll(content);

                    // Track transformed route info (for restoring originals)
                    // Note: we keep source in memory so we can restore it later
                    try transformed_routes.append(allocator, .{
                        .original_path = try allocator.dupe(u8, full_path),
                        .transformed_path = try allocator.dupe(u8, full_path), // Same as original
                        .content = source, // Keep original content for restoration (don't free)
                    });
                } else {
                    allocator.free(source);
                }
            },
            else => {},
        }
    }
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
        \\// Override any existing `declare module "*.svelte"` shims from dependencies.
        \\// We need strict module resolution: existing .svelte files have .svelte.ts siblings
        \\// that TypeScript resolves to, but missing files should error.
        \\// The `never` type for named exports ensures TypeScript errors on named imports
        \\// from non-existent .svelte files (default imports will still work as fallback).
        \\declare module "*.svelte" {
        \\  import type { Component } from "svelte";
        \\  const component: Component<any, any, any>;
        \\  export default component;
        \\  // Named exports typed as never to catch missing modules
        \\  export const _: never;
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
        \\// Note: $app/types is provided by @sveltejs/kit and project-specific
        \\// non-ambient.d.ts, so we don't redeclare it here to avoid overriding
        \\// the project's RouteId literals with our fallback `string` type.
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
    workspace_path: []const u8,
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
        // tsgo outputs paths in several formats depending on where the file is:
        // 1. ".zvelte-check/Component.svelte.ts" - relative to workspace
        // 2. "../../../tmp/file.svelte.ts" - relative paths for files outside workspace
        // 3. "/absolute/path/file.svelte.ts" - absolute paths
        //
        // Virtual paths are always absolute paths like "/path/to/file.svelte.ts".
        // We need to match the filename regardless of path prefix.
        //
        // Note: On Windows, tsgo may output backslashes. We handle both separators.
        const stub_prefix_slash = stub_dir ++ "/";
        const stub_prefix_backslash = stub_dir ++ "\\";

        // Normalize the path by:
        // 1. Stripping .zvelte-check/ prefix if present
        // 2. For paths with ../ prefixes (files outside workspace), extract filename
        var relative_stub_path = if (std.mem.startsWith(u8, file_path, stub_prefix_slash))
            file_path[stub_prefix_slash.len..]
        else if (std.mem.startsWith(u8, file_path, stub_prefix_backslash))
            file_path[stub_prefix_backslash.len..]
        else
            file_path;

        // For paths like "../../../tmp/file.svelte.ts", skip the ../ prefixes
        // These occur when tsgo runs from .zvelte-check/ but the file is outside
        // the workspace tree. We extract the meaningful path component.
        while (std.mem.startsWith(u8, relative_stub_path, "../")) {
            relative_stub_path = relative_stub_path[3..];
        }

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
        // Track whether this error is from script content (exact mapping) or
        // generated template code (fallback mapping). Template code loses type
        // narrowing context from {#if} blocks.
        var is_from_script = false;

        if (cached) |cf| {
            // This is a .svelte.ts file - map back to original .svelte
            original_path = cf.vf.original_path;

            // Use pre-computed line table to convert line/col → offset
            if (cf.ts_line_table.lineColToOffset(ts_line, ts_col)) |ts_offset| {
                // Map TS offset to Svelte offset with exactness tracking.
                // Exact mappings are from script content (verbatim copy).
                // Fallback mappings are from generated template code.
                const mapping = cf.vf.source_map.tsToSvelteWithExactness(ts_offset);
                is_from_script = mapping.is_exact;
                // Use pre-computed Svelte line table to convert offset → line/col
                const pos = cf.svelte_line_table.offsetToLineCol(mapping.offset);
                // Convert from 0-based to 1-based
                svelte_line = pos.line + 1;
                svelte_col = pos.col + 1;
            }
        } else {
            // This is a regular .ts/.js file - convert relative path to absolute path
            // tsgo runs from workspace root, so paths like "src/routes/..." are relative
            // to workspace. We need absolute paths to match main_workspace_files in main.zig.
            var relative_path = file_path;
            if (std.mem.startsWith(u8, file_path, "../")) {
                relative_path = file_path[3..]; // Strip "../" prefix if present
            }
            // Join workspace path with relative path to get absolute path
            original_path = try std.fs.path.join(allocator, &.{ workspace_path, relative_path });
        }

        // Parse severity and message
        const trimmed = std.mem.trim(u8, rest, " ");
        const is_error = std.mem.startsWith(u8, trimmed, "error");

        // Find message after code
        const msg_start = std.mem.indexOf(u8, trimmed, ": ");
        const message = if (msg_start) |s| trimmed[s + 2 ..] else trimmed;

        // Determine if this is a Svelte file (for Svelte-specific filtering)
        const is_svelte_file = std.mem.endsWith(u8, original_path, ".svelte");
        const is_sveltekit_route_file = isSvelteKitRouteFile(original_path);

        // Skip diagnostics from non-.svelte files UNLESS they are SvelteKit route files.
        // svelte-check reports errors from .svelte files AND from SvelteKit route files
        // (+page.ts, +page.server.ts, +layout.ts, +layout.server.ts, +server.ts).
        // Other .ts files are included in the tsconfig only for type resolution.
        if (!is_svelte_file and !is_sveltekit_route_file) continue;

        const is_test_file = isTestFile(original_path);
        // Check if this is a TypeScript Svelte file (has lang="ts" in script tag)
        const is_typescript_svelte = if (cached) |cf| cf.vf.is_typescript else false;

        // Skip errors that are false positives for Svelte files
        if (shouldSkipError(message, is_svelte_file, is_test_file, is_typescript_svelte, is_from_script)) continue;

        // Skip "Type assertion expressions can only be used in TypeScript files" for `as any`
        // The `as any` pattern is a common escape hatch that svelte-check doesn't report,
        // while `as SomeType` is reported. Check if the error points to `any` in the source.
        if (cached) |cf| {
            if (shouldSkipAsAnyTypeAssertion(message, cf.svelte_line_table, cf.vf.source_map.svelte_source, svelte_line, svelte_col)) {
                continue;
            }
        }

        // Transform "Module '*.svelte' has no exported member" errors to "Cannot find module"
        // When a .svelte import can't be resolved, TypeScript matches the wildcard module
        // declaration which gives a confusing error. Extract the actual module path from
        // the import statement in the source and report a clearer "Cannot find module" error.
        var final_message: []const u8 = message;
        if (cached) |cf| {
            if (transformMissingSvelteModuleError(allocator, message, cf.vf.source_map.svelte_source, cf.svelte_line_table, svelte_line)) |transformed| {
                final_message = transformed;
            }
        }

        try diagnostics.append(allocator, .{
            .source = .js,
            .severity = if (is_error) .@"error" else .warning,
            .code = null,
            .message = try allocator.dupe(u8, final_message),
            .file_path = try allocator.dupe(u8, original_path),
            .start_line = svelte_line,
            .start_col = svelte_col,
            .end_line = svelte_line,
            .end_col = svelte_col,
        });
    }
}

/// Checks if a type string represents a top-level union type.
/// Returns true if '|' appears at depth 0 (outside all brackets).
/// E.g., "A | B" -> true, "{ x: A | B }" -> false, "{ x: A } | B" -> true
fn isTopLevelUnionType(type_content: []const u8) bool {
    var brace_depth: u32 = 0; // { }
    var paren_depth: u32 = 0; // ( )
    var angle_depth: u32 = 0; // < >
    var bracket_depth: u32 = 0; // [ ]
    var i: usize = 0;

    while (i < type_content.len) {
        const c = type_content[i];

        // Track bracket depths
        if (c == '{') brace_depth += 1;
        if (c == '}') {
            if (brace_depth > 0) brace_depth -= 1;
        }
        if (c == '(') paren_depth += 1;
        if (c == ')') {
            if (paren_depth > 0) paren_depth -= 1;
        }
        if (c == '<') angle_depth += 1;
        if (c == '>') {
            if (angle_depth > 0) angle_depth -= 1;
        }
        if (c == '[') bracket_depth += 1;
        if (c == ']') {
            if (bracket_depth > 0) bracket_depth -= 1;
        }

        // Check for ' | ' at depth 0
        const total_depth = brace_depth + paren_depth + angle_depth + bracket_depth;
        if (total_depth == 0 and c == '|') {
            // Check for ' | ' pattern (space-pipe-space)
            if (i > 0 and i + 1 < type_content.len and
                type_content[i - 1] == ' ' and type_content[i + 1] == ' ')
            {
                return true;
            }
        }

        // Stop at the closing quote of the type (end of type content)
        if (c == '\'' and total_depth == 0 and i > 0) {
            break;
        }

        i += 1;
    }

    return false;
}

/// Returns true if the error should be skipped.
/// Some errors are Svelte-specific false positives (only skipped for .svelte files).
/// Others are general false positives from our code generation.
///
/// @param is_typescript_svelte: true if the .svelte file has lang="ts" in its script tag.
///        JavaScript Svelte files get less strict checking to match svelte-check behavior.
/// @param is_from_script: true if the error is from script content (exact source mapping),
///        false if from generated template code (fallback mapping). Template code loses
///        type narrowing context, so some errors are false positives there but not in scripts.
fn shouldSkipError(message: []const u8, is_svelte_file: bool, is_test_file: bool, is_typescript_svelte: bool, is_from_script: bool) bool {
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

        // Skip "Cannot redeclare block-scoped variable" errors.
        // False positives from:
        // - Snippet parameters shadowing outer variables
        // - Multiple {#each} blocks using the same loop variable name at module scope
        // - Script variables conflicting with template {@const} bindings
        // svelte-check doesn't report these because svelte2tsx handles scoping differently.
        if (std.mem.indexOf(u8, message, "Cannot redeclare block-scoped variable") != null) {
            return true;
        }

        // Skip "Duplicate identifier" errors.
        // False positives from type shadowing between module and instance scripts.
        // In Svelte, <script module> and <script> have separate scopes, but our
        // transformer emits both at module level in TypeScript.
        // svelte-check doesn't report these because svelte2tsx handles scoping differently.
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

        // NOTE: Previously we filtered "is possibly 'null'" and "is possibly 'undefined'" errors
        // for template code. This was because snippets incorrectly applied outer narrowing.
        // Now that snippets no longer apply outer enclosing if-conditions, we report these errors.
        // These are real null-safety errors that svelte-check correctly reports.

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
        // Only skip when the type involves a TOP-LEVEL union - NOT unions nested inside object types.
        // E.g., "Property 'error' does not exist on type '{ show: (options: string | ToastOptions) => ... }'"
        // has '|' inside a function parameter type, which is NOT a top-level union and should be reported.
        if (std.mem.indexOf(u8, message, "does not exist on type '") != null) {
            // Extract the type from the message and check if it's a union type
            // Real errors like "...does not exist on type '{}'." should NOT be skipped
            const type_start = std.mem.indexOf(u8, message, "does not exist on type '");
            if (type_start) |start| {
                const type_content = message[start + "does not exist on type '".len ..];
                // Skip if the type is '$$Props' - our generated type may be incomplete
                // (e.g., when using JSDoc instead of TypeScript type annotations)
                if (std.mem.startsWith(u8, type_content, "$$Props'.")) {
                    return true;
                }
                // Check for top-level union: '|' must appear at depth 0 (outside all brackets)
                if (isTopLevelUnionType(type_content)) {
                    return true;
                }
            }
        }

        // Skip "Argument of type 'X | null' is not assignable to parameter of type 'Y'"
        // False positives from template narrowing: {#if x} narrows x to be non-null,
        // but our transformer emits bindings at module scope without narrowing.
        // Only skip for template code - script code should report these as real errors.
        if (!is_from_script) {
            if (std.mem.indexOf(u8, message, "| null' is not assignable to parameter of type") != null) {
                return true;
            }
        }

        // Skip "Argument of type 'unknown' is not assignable" errors
        // These are cascade errors from missing type information (e.g., when ./$types
        // can't be resolved). The type chain `any` -> `unknown` via generic inference
        // produces these errors, but the root cause is already reported separately.
        if (std.mem.indexOf(u8, message, "Argument of type 'unknown'") != null) {
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

        // Skip "is not assignable to parameter of type" errors for Svelte's Immutable<> wrapper types.
        // Svelte wraps reactive values with Immutable<> which adds 'readonly' modifiers, causing
        // false positives when passing mutable values to component props.
        // Only skip if the message mentions Immutable<> specifically, not just any readonly type.
        // Real errors with 'readonly' object literals (like toolUse with wrong shape) should be reported.
        if (std.mem.indexOf(u8, message, "is not assignable to parameter of type") != null) {
            if (std.mem.indexOf(u8, message, "Immutable<") != null) {
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

        // NOTE: Previously we filtered "| undefined' is not assignable to type" errors, but this
        // caused false negatives for null-safety errors that svelte-check correctly reports.
        // The original filter was added because snippet bodies incorrectly applied outer narrowing.
        // Now that snippets no longer apply outer enclosing if-conditions, we report these errors.

        // Skip "Property X does not exist on type 'string'" errors.
        // False positives from {#each} loop variable shadowing: when multiple {#each} loops
        // use the same variable name (e.g., `member`), our transformer emits them with `var`
        // at module scope. TypeScript uses the first declaration's type, causing errors when
        // later loops have different element types with different properties.
        if (std.mem.indexOf(u8, message, "does not exist on type 'string'.") != null) {
            return true;
        }

        // Skip "Spread types may only be created from object types" errors.
        // False positives from component props validation when using spread with snippet parameters.
        // Example: {#snippet child({ props })} <Button {...props} /> creates a spread at module
        // scope where `props` isn't available (it's a snippet parameter). The spread would work
        // at runtime but our generated code emits it in a context where TypeScript can't see the type.
        if (std.mem.indexOf(u8, message, "Spread types may only be created from object types") != null) {
            return true;
        }

        // Skip "Cannot find name 'props'" errors for common snippet parameter names.
        // False positives from component props validation inside snippets like
        // {#snippet child({ props })} <Button {...props} />. The `props` identifier is extracted
        // from the spread but emitted at module scope where the snippet parameter isn't defined.
        if (std.mem.startsWith(u8, message, "Cannot find name 'props")) {
            return true;
        }

        // Skip "tabIndex does not exist... Did you mean tabindex?" errors.
        // Svelte accepts both camelCase (tabIndex) and lowercase (tabindex) attributes,
        // but svelte/elements types only define lowercase. This is a false positive.
        if (std.mem.indexOf(u8, message, "'tabIndex' does not exist") != null and
            std.mem.indexOf(u8, message, "tabindex") != null)
        {
            return true;
        }

        // NOTE: We used to skip "Parameter 'X' implicitly has an 'any' type" errors for
        // callback parameters, thinking they were cascade errors from untyped $props().
        // However, svelte-check DOES report these errors (verified with apply_patch-tool-use.svelte
        // which has `files.reduce((sum, file) => ...)` where `files` is `any[]`).
        // These are legitimate type errors that users should fix by adding explicit type annotations
        // to callback parameters. See comment at end of shouldSkipError for more context.

        // Skip "Element implicitly has an 'any' type because expression of type 'any'" errors.
        // When $props() returns any (e.g., SvelteKit pages without explicit types), expressions
        // like `typeLabels[entitlement.type]` fail because `entitlement.type` is `any`.
        // This is a cascade error from untyped $props() - svelte-check doesn't report it because
        // it uses SvelteKit's generated types to properly type the data.
        if (std.mem.indexOf(u8, message, "implicitly has an 'any' type because expression of type 'any'") != null) {
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

    // NOTE: We used to skip "Module '*.svelte' has no exported member" errors here,
    // but this hides real errors when importing from non-existent .svelte files.
    // The *.svelte wildcard module declaration now has `export const _: never` to
    // ensure TypeScript reports errors for named imports from missing .svelte files.
    // These errors indicate real missing modules and should NOT be suppressed.

    // Note: We do NOT skip "Parameter 'X' implicitly has an 'any' type" errors.
    // While some may be cascade errors from missing $types imports, many are real errors
    // (e.g., callback parameters on arrays with explicit `any[]` type, snippet parameters
    // without type annotations). svelte-check reports these, so we should too.

    // Skip "Untyped function calls may not accept type arguments" errors
    // These occur when the function's type can't be resolved (e.g., db.query<T>()
    // where db comes from platform.env which may not be typed). Since we don't
    // include all .ts files in our tsconfig, these are often false positives.
    if (std.mem.indexOf(u8, message, "Untyped function calls may not accept type arguments") != null) {
        return true;
    }

    // Skip "Conversion of type X to type Y may be a mistake" errors only for template literal types.
    // These are tsgo-specific false positives when mock data uses simplified keys like
    // 'thread-1' instead of full UUID template literals like 'T-${string}-...'
    // But don't skip legitimate errors like SVGForeignObjectElement -> HTMLElement casts.
    if (std.mem.indexOf(u8, message, "may be a mistake because neither type sufficiently overlaps") != null) {
        // Only skip if the error message involves template literal types (contains `${`)
        if (std.mem.indexOf(u8, message, "${") != null) {
            return true;
        }
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

/// Transform "Module '*.svelte' has no exported member 'X'" errors to "Cannot find module 'path'".
/// When a .svelte import can't be resolved, TypeScript matches the wildcard module declaration
/// and reports a confusing error. We extract the member name from the error message, then search
/// the source for an import that imports that member from a .svelte file.
///
/// Returns the transformed message, or null if the error doesn't match the pattern or
/// we can't find the module path.
fn transformMissingSvelteModuleError(
    allocator: std.mem.Allocator,
    message: []const u8,
    source: []const u8,
    line_table: LineTable,
    line: u32,
) ?[]const u8 {
    // Suppress unused parameter warning
    _ = line_table;
    _ = line;

    // Check if this is a "Module '*.svelte'" error and extract the member name
    // Error format: "Module '"*.svelte"' has no exported member 'X'."
    const prefix = "Module '\"*.svelte\"' has no exported member '";
    const start_idx = std.mem.indexOf(u8, message, prefix) orelse return null;
    const member_start = start_idx + prefix.len;
    const member_end = std.mem.indexOfPos(u8, message, member_start, "'") orelse return null;
    const member_name = message[member_start..member_end];

    // Search the entire source for an import that imports this member from a .svelte file
    // This avoids relying on potentially incorrect line number mapping (due to filtered imports)
    const module_path = findSvelteImportForMember(source, member_name) orelse return null;

    // Generate the transformed message
    return std.fmt.allocPrint(allocator, "Cannot find module '{s}' or its corresponding type declarations.", .{module_path}) catch null;
}

/// Searches the source for an import statement that imports the given member from a .svelte file.
/// Handles patterns like:
///   - `import X from './path.svelte'` (default import)
///   - `import { X } from './path.svelte'` (named import)
///   - `import type { X } from './path.svelte'` (type import)
///   - `import { type X, Y } from './path.svelte'` (mixed import with type specifier)
fn findSvelteImportForMember(source: []const u8, member_name: []const u8) ?[]const u8 {
    var i: usize = 0;
    while (i < source.len) {
        // Find next line
        const line_start = i;
        var line_end = i;
        while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}
        const line = source[line_start..line_end];

        // Check if this is an import from a .svelte file
        if (std.mem.indexOf(u8, line, ".svelte") != null) {
            if (importLineHasMember(line, member_name)) {
                // Extract the module path
                if (extractModulePathFromImport(line)) |path| {
                    if (std.mem.endsWith(u8, path, ".svelte")) {
                        return path;
                    }
                }
            }
        }

        i = line_end;
        if (i < source.len and source[i] == '\n') i += 1;
    }
    return null;
}

/// Checks if an import line imports the given member name.
/// Handles default imports, named imports, and type imports.
fn importLineHasMember(line: []const u8, member_name: []const u8) bool {
    const trimmed = std.mem.trim(u8, line, " \t\r");

    // Must be an import statement
    if (!std.mem.startsWith(u8, trimmed, "import")) return false;

    // Find "from " to isolate the import clause
    const from_idx = std.mem.indexOf(u8, trimmed, " from ") orelse return false;
    const import_clause = trimmed["import".len..from_idx];

    // Check for named imports: { X } or { type X } or { X, Y, type Z }
    if (std.mem.indexOf(u8, import_clause, "{")) |brace_start| {
        if (std.mem.indexOf(u8, import_clause, "}")) |brace_end| {
            if (brace_start < brace_end) {
                const specifiers = import_clause[brace_start + 1 .. brace_end];
                // Check each specifier
                var iter = std.mem.splitScalar(u8, specifiers, ',');
                while (iter.next()) |spec_raw| {
                    var spec = std.mem.trim(u8, spec_raw, " \t\r\n");
                    // Remove "type " prefix if present
                    if (std.mem.startsWith(u8, spec, "type ")) {
                        spec = std.mem.trim(u8, spec["type ".len..], " \t");
                    }
                    // Handle "X as Y" - we want to match X
                    if (std.mem.indexOf(u8, spec, " as ")) |as_idx| {
                        spec = std.mem.trim(u8, spec[0..as_idx], " \t");
                    }
                    if (std.mem.eql(u8, spec, member_name)) return true;
                }
            }
        }
    }

    // Check for default import: import X from './path.svelte'
    // The clause would be " X " or " type X "
    const clause_trimmed = std.mem.trim(u8, import_clause, " \t");

    // Skip "type " prefix for type imports
    var default_name = clause_trimmed;
    if (std.mem.startsWith(u8, default_name, "type ")) {
        default_name = std.mem.trim(u8, default_name["type ".len..], " \t");
    }

    // If there's no { }, this is a default import - check if name matches
    if (std.mem.indexOf(u8, default_name, "{") == null) {
        // Handle "* as X" namespace imports
        if (std.mem.startsWith(u8, default_name, "* as ")) {
            default_name = std.mem.trim(u8, default_name["* as ".len..], " \t");
        }
        if (std.mem.eql(u8, default_name, member_name)) return true;
    }

    return false;
}

/// Extract the module path from an import statement.
/// Handles: import X from 'path', import { X } from 'path', import type { X } from 'path'
fn extractModulePathFromImport(line: []const u8) ?[]const u8 {
    // Look for "from " followed by a quoted string
    const from_idx = std.mem.indexOf(u8, line, " from ") orelse
        std.mem.indexOf(u8, line, "\tfrom ") orelse
        return null;

    const after_from = line[from_idx + " from ".len ..];

    // Find the opening quote (single or double)
    const quote_char: u8 = blk: {
        for (after_from) |c| {
            if (c == '\'' or c == '"') break :blk c;
            if (c != ' ' and c != '\t') return null; // Non-whitespace before quote
        }
        return null;
    };

    const quote_start = std.mem.indexOfScalar(u8, after_from, quote_char) orelse return null;
    const path_start = quote_start + 1;

    // Find the closing quote
    const quote_end = std.mem.indexOfScalarPos(u8, after_from, path_start, quote_char) orelse return null;

    return after_from[path_start..quote_end];
}

/// Returns true if the path is a test file.
/// Matches common test file patterns: .test.ts, .test.js, .spec.ts, .spec.js
fn isTestFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".test.ts") or
        std.mem.endsWith(u8, path, ".test.js") or
        std.mem.endsWith(u8, path, ".spec.ts") or
        std.mem.endsWith(u8, path, ".spec.js");
}

/// Returns true if the path is a SvelteKit route file.
/// These files are checked alongside .svelte files by svelte-check.
fn isSvelteKitRouteFile(path: []const u8) bool {
    // Extract filename from path
    const filename = if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx|
        path[idx + 1 ..]
    else if (std.mem.lastIndexOfScalar(u8, path, '\\')) |idx|
        path[idx + 1 ..]
    else
        path;

    // SvelteKit route files: +page.ts, +page.server.ts, +layout.ts, +layout.server.ts, +server.ts
    return std.mem.eql(u8, filename, "+page.ts") or
        std.mem.eql(u8, filename, "+page.server.ts") or
        std.mem.eql(u8, filename, "+layout.ts") or
        std.mem.eql(u8, filename, "+layout.server.ts") or
        std.mem.eql(u8, filename, "+server.ts");
}

test "isSvelteKitRouteFile identifies SvelteKit route files" {
    // Positive cases - should match
    try std.testing.expect(isSvelteKitRouteFile("src/routes/+page.ts"));
    try std.testing.expect(isSvelteKitRouteFile("src/routes/+page.server.ts"));
    try std.testing.expect(isSvelteKitRouteFile("src/routes/admin/+layout.ts"));
    try std.testing.expect(isSvelteKitRouteFile("src/routes/admin/+layout.server.ts"));
    try std.testing.expect(isSvelteKitRouteFile("src/routes/api/+server.ts"));
    try std.testing.expect(isSvelteKitRouteFile("+page.ts")); // Just filename

    // Negative cases - should NOT match
    try std.testing.expect(!isSvelteKitRouteFile("src/lib/utils.ts"));
    try std.testing.expect(!isSvelteKitRouteFile("src/routes/page.ts")); // Missing +
    try std.testing.expect(!isSvelteKitRouteFile("src/routes/+page.svelte")); // Wrong extension
    try std.testing.expect(!isSvelteKitRouteFile("src/hooks.server.ts")); // hooks.server.ts is not a route file
    try std.testing.expect(!isSvelteKitRouteFile("src/routes/settings.remote.ts")); // *.remote.ts is not a route file
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

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, ".", &diagnostics);

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

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, ".", &diagnostics);

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

    try parseTsgoOutput(allocator, tsgo_output, &cached_files, ".", &diagnostics);

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
    const empty_routes: []const TransformedRouteFile = &.{};
    try writeGeneratedTsconfig(workspace_dir, null, &virtual_files, empty_routes);

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
    const empty_routes: []const TransformedRouteFile = &.{};
    try writeGeneratedTsconfig(workspace_dir, null, &virtual_files, empty_routes);

    // Read generated config
    const content = try workspace_dir.readFileAlloc(allocator, generated_tsconfig, 10 * 1024);

    // Verify extends uses ../ prefix (tsconfig is in .zvelte-check/ subdirectory)
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\": \"../tsconfig.json\"") != null);
}

test "extractModulePathFromImport handles various import styles" {
    // Named import with single quotes
    try std.testing.expectEqualStrings(
        "./component.svelte",
        extractModulePathFromImport("import { Foo } from './component.svelte';").?,
    );

    // Named import with double quotes
    try std.testing.expectEqualStrings(
        "./component.svelte",
        extractModulePathFromImport("import { Foo } from \"./component.svelte\";").?,
    );

    // Default import
    try std.testing.expectEqualStrings(
        "../other.svelte",
        extractModulePathFromImport("import Foo from '../other.svelte';").?,
    );

    // Type import
    try std.testing.expectEqualStrings(
        "$lib/components/foo.svelte",
        extractModulePathFromImport("import type { Bar } from '$lib/components/foo.svelte';").?,
    );

    // Multiple named imports
    try std.testing.expectEqualStrings(
        "./utils.svelte",
        extractModulePathFromImport("import { a, b, c } from './utils.svelte';").?,
    );

    // No from clause
    try std.testing.expect(extractModulePathFromImport("const x = 1;") == null);

    // Export instead of import
    try std.testing.expect(extractModulePathFromImport("export { Foo }") == null);
}

test "transformMissingSvelteModuleError transforms wildcard errors" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Source with an import statement on line 3
    const source =
        \\<script lang="ts">
        \\  // Comment
        \\  import { Foo } from './nonexistent.svelte';
        \\</script>
    ;

    const line_table = try LineTable.init(allocator, source);

    // Message that matches the pattern
    const message = "Module '\"*.svelte\"' has no exported member 'Foo'. Did you mean to use 'import Foo from \"*.svelte\"' instead?";

    // Error is on line 3 (1-based)
    const result = transformMissingSvelteModuleError(allocator, message, source, line_table, 3);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(
        "Cannot find module './nonexistent.svelte' or its corresponding type declarations.",
        result.?,
    );
}

test "transformMissingSvelteModuleError returns null for non-matching messages" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "import { Foo } from './exists.svelte';";
    const line_table = try LineTable.init(allocator, source);

    // Different error message
    const message = "Type 'string' is not assignable to type 'number'.";
    const result = transformMissingSvelteModuleError(allocator, message, source, line_table, 1);

    try std.testing.expect(result == null);
}

test "transformMissingSvelteModuleError returns null for non-svelte imports" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Import a .ts file (not .svelte)
    const source = "import { Foo } from './utils.ts';";
    const line_table = try LineTable.init(allocator, source);

    // Even if the error message matches the pattern, we shouldn't transform non-.svelte imports
    const message = "Module '\"*.svelte\"' has no exported member 'Foo'.";
    const result = transformMissingSvelteModuleError(allocator, message, source, line_table, 1);

    try std.testing.expect(result == null);
}

test "transformMissingSvelteModuleError finds member across multiple imports" {
    // Regression test: when filtered imports cause line number misalignment,
    // we should still find the correct module by searching for the member name
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Source has multiple svelte imports; the error is about ThreadRestoration
    const source =
        \\<script lang="ts">
        \\  import { type Snippet } from 'svelte'
        \\  import PromptsTOC from './prompts-toc.svelte'
        \\  import type { ThreadRestoration } from './restoration-state.svelte'
        \\</script>
    ;

    const line_table = try LineTable.init(allocator, source);

    // Error message about ThreadRestoration - even if line number is wrong (2 instead of 4),
    // we should find the correct module path by searching for the member name
    const message = "Module '\"*.svelte\"' has no exported member 'ThreadRestoration'. Did you mean to use 'import ThreadRestoration from \"*.svelte\"' instead?";

    // Pass wrong line number (2) to simulate line number misalignment from filtered imports
    const result = transformMissingSvelteModuleError(allocator, message, source, line_table, 2);

    try std.testing.expect(result != null);
    // Should find restoration-state.svelte (where ThreadRestoration is imported from),
    // NOT prompts-toc.svelte (which was on the wrong line)
    try std.testing.expectEqualStrings(
        "Cannot find module './restoration-state.svelte' or its corresponding type declarations.",
        result.?,
    );
}

test "findSvelteImportForMember handles default and named imports" {
    // Default import
    try std.testing.expect(findSvelteImportForMember(
        "import Foo from './foo.svelte'",
        "Foo",
    ) != null);
    try std.testing.expectEqualStrings(
        "./foo.svelte",
        findSvelteImportForMember("import Foo from './foo.svelte'", "Foo").?,
    );

    // Named import
    try std.testing.expectEqualStrings(
        "./foo.svelte",
        findSvelteImportForMember("import { Bar } from './foo.svelte'", "Bar").?,
    );

    // Type import
    try std.testing.expectEqualStrings(
        "./foo.svelte",
        findSvelteImportForMember("import type { Baz } from './foo.svelte'", "Baz").?,
    );

    // Mixed import with type specifier
    try std.testing.expectEqualStrings(
        "./foo.svelte",
        findSvelteImportForMember("import { type Qux, Other } from './foo.svelte'", "Qux").?,
    );

    // Non-matching member
    try std.testing.expect(findSvelteImportForMember(
        "import Foo from './foo.svelte'",
        "Bar",
    ) == null);

    // Non-svelte import
    try std.testing.expect(findSvelteImportForMember(
        "import Foo from './foo.ts'",
        "Foo",
    ) == null);
}

test "importLineHasMember handles various import patterns" {
    // Default import
    try std.testing.expect(importLineHasMember("import Foo from './foo.svelte'", "Foo"));
    try std.testing.expect(!importLineHasMember("import Foo from './foo.svelte'", "Bar"));

    // Named import
    try std.testing.expect(importLineHasMember("import { Foo } from './foo.svelte'", "Foo"));
    try std.testing.expect(importLineHasMember("import { Foo, Bar } from './foo.svelte'", "Bar"));

    // Type import
    try std.testing.expect(importLineHasMember("import type { Foo } from './foo.svelte'", "Foo"));

    // Mixed import with type specifier
    try std.testing.expect(importLineHasMember("import { type Foo, Bar } from './foo.svelte'", "Foo"));
    try std.testing.expect(importLineHasMember("import { type Foo, Bar } from './foo.svelte'", "Bar"));

    // Aliased import (X as Y - should match X, not Y)
    try std.testing.expect(importLineHasMember("import { Foo as F } from './foo.svelte'", "Foo"));
    try std.testing.expect(!importLineHasMember("import { Foo as F } from './foo.svelte'", "F"));

    // Not an import
    try std.testing.expect(!importLineHasMember("const Foo = 1;", "Foo"));
}
