//! svelte-check-zig: A fast Svelte diagnostic tool
//!
//! Pipeline:
//! 1. Scan workspace for .svelte files
//! 2. Parse each file (parallel via thread pool)
//! 3. Run Svelte diagnostics (a11y, CSS)
//! 4. Transform to virtual .ts files
//! 5. Run tsgo on all files
//! 6. Map TS errors back to Svelte positions
//! 7. Output results

const std = @import("std");
const builtin = @import("builtin");

pub const cli = @import("cli.zig");
pub const workspace = @import("workspace.zig");
pub const Lexer = @import("svelte_lexer.zig").Lexer;
pub const Parser = @import("svelte_parser.zig").Parser;
pub const transformer = @import("transformer.zig");
pub const tsgo = @import("tsgo.zig");
pub const tsconfig = @import("tsconfig.zig");
pub const source_map = @import("source_map.zig");
pub const output = @import("output.zig");
pub const Diagnostic = @import("diagnostic.zig").Diagnostic;
pub const sveltekit = @import("sveltekit.zig");

/// Result of processing a single file
const FileResult = struct {
    virtual_file: transformer.VirtualFile,
    diagnostics: []Diagnostic,
    arena: ?std.heap.ArenaAllocator = null,
    err: ?anyerror = null,
};

/// Shared context for parallel file processing
const ProcessContext = struct {
    backing_allocator: std.mem.Allocator,
    results: []FileResult,
    diagnostic_sources: cli.DiagnosticSources,
};

pub fn main() !void {
    // Use DebugAllocator in debug/safe modes, SmpAllocator in release
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const backing_allocator, const is_debug = switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    // Use arena for all run-time allocations - freed at end of main
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = cli.parseArgs(allocator) catch |err| {
        const stderr = std.fs.File.stderr();
        switch (err) {
            error.UnknownArgument => stderr.writeAll("error: unknown argument\n\nRun with --help for usage.\n") catch {},
            error.MissingArgValue => stderr.writeAll("error: missing value for argument\n") catch {},
            error.WatchModeNotSupported => stderr.writeAll("error: watch mode is not supported\n") catch {},
            else => stderr.writeAll("error: invalid arguments\n") catch {},
        }
        std.process.exit(1);
    };

    const exit_code = try run(backing_allocator, allocator, args);
    std.process.exit(exit_code);
}

fn run(backing_allocator: std.mem.Allocator, allocator: std.mem.Allocator, args: cli.Args) !u8 {
    const stdout = std.fs.File.stdout();
    var msg_buf: [512]u8 = undefined;

    // 1. Load tsconfig for file filtering (unless --no-tsconfig)
    const config = if (!args.no_tsconfig)
        try tsconfig.load(allocator, args.workspace, args.tsconfig)
    else
        null;

    // 2. Scan workspace and filter by tsconfig patterns
    const all_files = try workspace.scan(allocator, args.workspace, args.ignore);

    // Filter files by tsconfig include/exclude patterns
    const files = if (config) |cfg| blk: {
        var filtered: std.ArrayList([]const u8) = .empty;
        for (all_files) |file_path| {
            // Normalize path for pattern matching:
            // 1. Strip workspace prefix to get relative path
            // 2. Strip ./ prefix if present
            const relative = if (std.mem.startsWith(u8, file_path, args.workspace))
                file_path[args.workspace.len..]
            else
                file_path;
            const normalized = if (std.mem.startsWith(u8, relative, "/"))
                relative[1..]
            else if (std.mem.startsWith(u8, relative, "./"))
                relative[2..]
            else
                relative;
            if (tsconfig.shouldInclude(cfg, normalized)) {
                try filtered.append(allocator, file_path);
            }
        }
        break :blk try filtered.toOwnedSlice(allocator);
    } else all_files;

    if (files.len == 0) {
        try output.printNoFiles(args.output_format);
        return 0;
    }

    if (args.output_format == .human_verbose) {
        const msg = std.fmt.bufPrint(&msg_buf, "Found {d} Svelte file(s)\n\n", .{files.len}) catch "";
        try stdout.writeAll(msg);
    }

    // 2-4. Parse, diagnose, transform (parallel)
    // Use backing_allocator for thread pool (arena isn't thread-safe)
    // Each thread gets its own arena for allocations
    const results = try backing_allocator.alloc(FileResult, files.len);
    defer backing_allocator.free(results);
    @memset(results, .{
        .virtual_file = .{
            .original_path = "",
            .virtual_path = "",
            .content = "",
            .source_map = .{ .mappings = &.{}, .svelte_source = "" },
        },
        .diagnostics = &.{},
        .arena = null,
        .err = null,
    });

    var ctx: ProcessContext = .{
        .backing_allocator = backing_allocator,
        .results = results,
        .diagnostic_sources = args.diagnostic_sources,
    };

    // Use thread pool for parallel processing
    const cpu_count = std.Thread.getCpuCount() catch 1;
    const n_jobs = @max(1, cpu_count -| 1);

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{
        .allocator = backing_allocator,
        .n_jobs = n_jobs,
    });
    defer pool.deinit();

    var wg: std.Thread.WaitGroup = .{};

    for (files, 0..) |file_path, i| {
        pool.spawnWg(&wg, processFile, .{ &ctx, file_path, i });
    }

    wg.wait();

    // Collect results and clean up per-thread arenas
    defer for (results) |*result| {
        if (result.arena) |*arena| {
            arena.deinit();
        }
    };

    var all_diagnostics: std.ArrayList(Diagnostic) = .empty;
    var virtual_files: std.ArrayList(transformer.VirtualFile) = .empty;

    for (results) |result| {
        if (result.err) |err| {
            return err;
        }
        try virtual_files.append(allocator, result.virtual_file);
        for (result.diagnostics) |d| {
            try all_diagnostics.append(allocator, d);
        }
    }

    if (args.output_format == .human_verbose) {
        for (virtual_files.items) |virtual| {
            const msg = std.fmt.bufPrint(&msg_buf, "{s}: generated TS ({d} bytes)\n", .{
                virtual.original_path,
                virtual.content.len,
            }) catch "";
            try stdout.writeAll(msg);
        }
        try stdout.writeAll("\n");
    }

    // 5. Run tsgo (skip when js diagnostics disabled or --no-tsconfig)
    if (!args.no_tsconfig and args.diagnostic_sources.js) {
        const ts_diagnostics = tsgo.check(allocator, virtual_files.items, args.workspace, args.tsconfig) catch |err| {
            if (err == tsgo.TsgoNotFoundError.TsgoNotFound) {
                const stderr = std.fs.File.stderr();
                try stderr.writeAll("error: tsgo not found\n\n");
                try stderr.writeAll("tsgo is required for TypeScript checking. Install it with:\n");
                try stderr.writeAll("  pnpm add -D @aspect-build/tsgo\n\n");
                try stderr.writeAll("Or skip TypeScript checking with --no-tsconfig\n");
                return 1;
            }
            return err;
        };

        for (ts_diagnostics) |d| {
            try all_diagnostics.append(allocator, d);
        }
    }

    // 6. Output
    try output.print(allocator, args.output_format, all_diagnostics.items, args.threshold, args.compiler_warnings);

    // 7. Exit code - apply compiler warnings to determine exit status
    var has_errors = false;
    var has_warnings = false;
    for (all_diagnostics.items) |d| {
        var severity = d.severity;
        if (d.code) |code| {
            if (args.compiler_warnings.get(code)) |behavior| {
                switch (behavior) {
                    .ignore => continue,
                    .@"error" => severity = .@"error",
                }
            }
        }
        if (severity == .@"error") has_errors = true;
        if (severity == .warning) has_warnings = true;
        if (has_errors and has_warnings) break;
    }

    if (has_errors) return 1;
    if (has_warnings and args.fail_on_warnings) return 1;
    return 0;
}

fn processFile(ctx: *ProcessContext, file_path: []const u8, index: usize) void {
    processFileInner(ctx, file_path, index) catch |err| {
        ctx.results[index].err = err;
    };
}

fn processFileInner(ctx: *ProcessContext, file_path: []const u8, index: usize) !void {
    // Each thread gets its own arena (avoids lock contention)
    var arena = std.heap.ArenaAllocator.init(ctx.backing_allocator);
    errdefer arena.deinit();
    const allocator = arena.allocator();

    // Read file
    const source = try std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024);

    // Parse
    var parser: Parser = .init(allocator, source, file_path);
    const ast = try parser.parse();

    // Svelte diagnostics (a11y, CSS) - collect into local list
    var diag_list: std.ArrayList(Diagnostic) = .empty;
    try ast.runDiagnostics(allocator, &diag_list, .{
        .svelte = ctx.diagnostic_sources.svelte,
        .css = ctx.diagnostic_sources.css,
    });

    // Transform to TS
    const virtual = try transformer.transform(allocator, ast);

    // Store result (no lock needed - each thread writes to unique index)
    ctx.results[index] = .{
        .virtual_file = virtual,
        .diagnostics = try diag_list.toOwnedSlice(allocator),
        .arena = arena,
    };
}

pub const integration_test = @import("integration_test.zig");

test {
    std.testing.refAllDecls(@This());
}
