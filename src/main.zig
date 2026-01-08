//! svelte-check-zig: A fast Svelte diagnostic tool
//!
//! Pipeline:
//! 1. Scan workspace for .svelte files
//! 2. Parse each file (parallel)
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
pub const source_map = @import("source_map.zig");
pub const output = @import("output.zig");
pub const Diagnostic = @import("diagnostic.zig").Diagnostic;

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

    const args = try cli.parseArgs(allocator);

    const exit_code = try run(allocator, args);
    std.process.exit(exit_code);
}

fn run(allocator: std.mem.Allocator, args: cli.Args) !u8 {
    const stdout = std.fs.File.stdout();
    var msg_buf: [512]u8 = undefined;

    // 1. Scan workspace
    const files = try workspace.scan(allocator, args.workspace, args.ignore);

    if (files.len == 0) {
        try output.printNoFiles(args.output_format);
        return 0;
    }

    if (args.output_format == .human_verbose) {
        const msg = std.fmt.bufPrint(&msg_buf, "Found {d} Svelte file(s)\n\n", .{files.len}) catch "";
        try stdout.writeAll(msg);
    }

    // 2-4. Parse, diagnose, transform (TODO: parallelize)
    var all_diagnostics: std.ArrayList(Diagnostic) = .empty;
    var virtual_files: std.ArrayList(transformer.VirtualFile) = .empty;

    for (files) |file_path| {
        const source = try std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024);

        // Parse
        var parser: Parser = .init(allocator, source, file_path);
        const ast = try parser.parse();

        if (args.output_format == .human_verbose) {
            const msg = std.fmt.bufPrint(&msg_buf, "{s}: {d} nodes, {d} scripts, {d} styles\n", .{
                file_path,
                ast.nodes.items.len,
                ast.scripts.items.len,
                ast.styles.items.len,
            }) catch "";
            try stdout.writeAll(msg);
        }

        // Svelte diagnostics (a11y, CSS)
        try ast.runDiagnostics(allocator, &all_diagnostics);

        // Transform to TS
        const virtual = try transformer.transform(allocator, ast);
        try virtual_files.append(allocator, virtual);

        if (args.output_format == .human_verbose) {
            try stdout.writeAll("  Generated TS:\n");
            // Show first 200 chars of generated TS
            const preview_len = @min(virtual.content.len, 200);
            try stdout.writeAll("  ");
            for (virtual.content[0..preview_len]) |c| {
                if (c == '\n') {
                    try stdout.writeAll("\n  ");
                } else {
                    try stdout.writeAll(&.{c});
                }
            }
            if (virtual.content.len > 200) try stdout.writeAll("...");
            try stdout.writeAll("\n\n");
        }
    }

    // 5. Run tsgo
    if (!args.no_tsconfig) {
        const ts_diagnostics = try tsgo.check(allocator, virtual_files.items, args.tsconfig);

        for (ts_diagnostics) |d| {
            try all_diagnostics.append(allocator, d);
        }
    }

    // 6. Output
    try output.print(allocator, args.output_format, all_diagnostics.items, args.threshold);

    // 7. Exit code
    const has_errors = for (all_diagnostics.items) |d| {
        if (d.severity == .@"error") break true;
    } else false;

    const has_warnings = for (all_diagnostics.items) |d| {
        if (d.severity == .warning) break true;
    } else false;

    if (has_errors) return 1;
    if (has_warnings and args.fail_on_warnings) return 1;
    return 0;
}

test {
    std.testing.refAllDecls(@This());
}
