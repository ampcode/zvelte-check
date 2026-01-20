//! Dump zvelte-check transformer output for a Svelte file.
//!
//! Usage: zig build run-dump -- <file.svelte>
//!
//! This outputs the generated TypeScript that gets sent to tsgo for type-checking.
//! Useful for comparing against svelte2tsx output when debugging false positives.

const std = @import("std");
const Parser = @import("svelte_parser.zig");
const transformer = @import("transformer.zig");

pub fn main() !void {
    // Use arena allocator - no need to free individual allocations
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file.svelte>\n", .{args[0]});
        std.debug.print("\nDumps the generated TypeScript for debugging.\n", .{});
        std.debug.print("Compare with svelte2tsx output to find transformer bugs.\n", .{});
        std.process.exit(1);
    }

    const file_path = args[1];

    // Read the file
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        std.debug.print("Error: cannot open '{s}': {}\n", .{ file_path, err });
        std.process.exit(1);
    };
    defer file.close();

    const source = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error: cannot read '{s}': {}\n", .{ file_path, err });
        std.process.exit(1);
    };
    defer allocator.free(source);

    // Parse
    var parser = Parser.Parser.init(allocator, source, file_path);
    const ast = parser.parse() catch |err| {
        std.debug.print("Error: parse failed: {}\n", .{err});
        std.process.exit(1);
    };

    // Transform
    const virtual = transformer.transform(allocator, ast) catch |err| {
        std.debug.print("Error: transform failed: {}\n", .{err});
        std.process.exit(1);
    };

    // Output
    const stdout = std.fs.File.stdout();
    var header_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Generated from {s}
        \\// Virtual path: {s}
        \\// Is TypeScript: {}
        \\// Source map entries: {d}
        \\
        \\
    , .{ file_path, virtual.virtual_path, virtual.is_typescript, virtual.source_map.mappings.len }) catch "";
    try stdout.writeAll(header);
    try stdout.writeAll(virtual.content);
    try stdout.writeAll("\n");
}
