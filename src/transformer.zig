//! Svelte → TypeScript transformer
//!
//! Generates virtual .svelte.ts files that tsgo can type-check.
//! Also builds source maps for error position mapping.

const std = @import("std");
const Ast = @import("svelte_parser.zig").Ast;
const ScriptData = @import("svelte_parser.zig").ScriptData;
const SourceMap = @import("source_map.zig").SourceMap;

pub const VirtualFile = struct {
    original_path: []const u8,
    virtual_path: []const u8, // path.svelte.ts
    content: []const u8,
    source_map: SourceMap,
};

pub fn transform(allocator: std.mem.Allocator, ast: Ast) !VirtualFile {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var mappings: std.ArrayList(SourceMap.Mapping) = .empty;
    defer mappings.deinit(allocator);

    // Generate imports and setup
    try output.appendSlice(allocator, "// Generated from ");
    try output.appendSlice(allocator, ast.file_path);
    try output.appendSlice(allocator, "\n\n");

    // Extract and emit script content
    for (ast.scripts.items) |script| {
        const content = ast.source[script.content_start..script.content_end];

        // Track mapping
        try mappings.append(allocator, .{
            .svelte_offset = script.content_start,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(content.len),
        });

        try output.appendSlice(allocator, content);
        try output.appendSlice(allocator, "\n\n");
    }

    // Generate component type info for props, slots, etc.
    try output.appendSlice(allocator, "// Component exports\n");
    try output.appendSlice(allocator, "export default {};\n");

    // Build virtual path
    const virtual_path = try std.fmt.allocPrint(allocator, "{s}.ts", .{ast.file_path});

    return .{
        .original_path = ast.file_path,
        .virtual_path = virtual_path,
        .content = try output.toOwnedSlice(allocator),
        .source_map = .{
            .mappings = try mappings.toOwnedSlice(allocator),
            .svelte_source = ast.source,
        },
    };
}

test "transform simple script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script>
        \\  let count = 0;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    try std.testing.expect(virtual.content.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let count = 0") != null);
}
