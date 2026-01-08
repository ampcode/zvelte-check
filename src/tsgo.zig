//! tsgo runner - spawns tsgo for TypeScript type-checking
//!
//! Writes virtual files to temp dir, runs tsgo, parses output.

const std = @import("std");
const VirtualFile = @import("transformer.zig").VirtualFile;
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const SourceMap = @import("source_map.zig").SourceMap;
const LineTable = @import("source_map.zig").LineTable;

pub fn check(
    allocator: std.mem.Allocator,
    virtual_files: []const VirtualFile,
    tsconfig_path: ?[]const u8,
) ![]Diagnostic {
    if (virtual_files.len == 0) return &.{};

    // Create temp directory for virtual files
    var tmp_dir = try std.fs.cwd().makeOpenPath(".svelte-check-zig-tmp", .{});
    defer {
        std.fs.cwd().deleteTree(".svelte-check-zig-tmp") catch {};
    }
    defer tmp_dir.close();

    // Write virtual files
    for (virtual_files) |vf| {
        const basename = std.fs.path.basename(vf.virtual_path);
        const file = try tmp_dir.createFile(basename, .{});
        defer file.close();
        try file.writeAll(vf.content);
    }

    // Build tsgo command
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    try args.append(allocator, "tsgo");
    try args.append(allocator, "--noEmit");

    // Add all virtual files (don't use --project with files)
    _ = tsconfig_path; // TODO: use tsconfig for compiler options
    for (virtual_files) |vf| {
        const basename = std.fs.path.basename(vf.virtual_path);
        const tmp_path = try std.fs.path.join(allocator, &.{ ".svelte-check-zig-tmp", basename });
        try args.append(allocator, try allocator.dupe(u8, tmp_path));
    }

    // Run tsgo
    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    try child.spawn();

    const stdout = try child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(stdout);

    const stderr = try child.stderr.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(stderr);

    const result = try child.wait();

    // Parse tsgo output
    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    // tsgo outputs diagnostics to stdout, not stderr
    const has_output = stdout.len > 0 or stderr.len > 0;
    const exited_with_error = switch (result) {
        .Exited => |code| code != 0,
        else => true,
    };

    if (exited_with_error or has_output) {
        // tsgo outputs to stdout
        try parseTsgoOutput(allocator, stdout, virtual_files, &diagnostics);
        // Also check stderr for any errors
        if (stderr.len > 0) {
            try parseTsgoOutput(allocator, stderr, virtual_files, &diagnostics);
        }
    }

    return diagnostics.toOwnedSlice(allocator);
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

        // Find corresponding virtual file
        const vf: ?VirtualFile = for (virtual_files) |v| {
            if (std.mem.endsWith(u8, file_path, std.fs.path.basename(v.virtual_path))) {
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
            .code = null, // TODO: extract TSxxxx code
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

    const tsgo_output = "test.svelte.ts(5,10): error TS2322: Type 'string' is not assignable to type 'number'.\n";

    var diagnostics: std.ArrayList(Diagnostic) = .empty;

    const virtual_files = [_]VirtualFile{.{
        .original_path = "test.svelte",
        .virtual_path = "test.svelte.ts",
        .content = "",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    try parseTsgoOutput(allocator, tsgo_output, &virtual_files, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
}
