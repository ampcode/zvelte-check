//! tsgo runner - spawns tsgo for TypeScript type-checking
//!
//! Writes virtual files to temp dir, runs tsgo, parses output.
//! Generates a temporary tsconfig.json that extends the project's config
//! to inherit path aliases ($lib, $app, etc.) for module resolution.

const std = @import("std");
const VirtualFile = @import("transformer.zig").VirtualFile;
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const SourceMap = @import("source_map.zig").SourceMap;
const LineTable = @import("source_map.zig").LineTable;

pub fn check(
    allocator: std.mem.Allocator,
    virtual_files: []const VirtualFile,
    workspace_path: []const u8,
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

    // Generate tsconfig.json in temp dir that extends the project's config
    const has_project_config = try writeTempTsconfig(allocator, tmp_dir, workspace_path, tsconfig_path, virtual_files);

    // Build tsgo command
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    try args.append(allocator, "tsgo");
    try args.append(allocator, "--noEmit");

    if (has_project_config) {
        // Use project mode to pick up path aliases
        try args.append(allocator, "--project");
        try args.append(allocator, ".svelte-check-zig-tmp/tsconfig.json");
    } else {
        // No tsconfig - pass files directly
        for (virtual_files) |vf| {
            const basename = std.fs.path.basename(vf.virtual_path);
            const tmp_path = try std.fs.path.join(allocator, &.{ ".svelte-check-zig-tmp", basename });
            try args.append(allocator, try allocator.dupe(u8, tmp_path));
        }
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

/// Generates a tsconfig.json in the temp directory.
/// If a project tsconfig exists, extends it to inherit path aliases.
/// Returns true if a tsconfig was written.
fn writeTempTsconfig(
    allocator: std.mem.Allocator,
    tmp_dir: std.fs.Dir,
    workspace_path: []const u8,
    tsconfig_path: ?[]const u8,
    virtual_files: []const VirtualFile,
) !bool {
    // Determine the project tsconfig path
    const project_tsconfig = tsconfig_path orelse blk: {
        // Auto-detect tsconfig.json in workspace root
        const candidate = try std.fs.path.join(allocator, &.{ workspace_path, "tsconfig.json" });
        const cwd = std.fs.cwd();
        if (cwd.access(candidate, .{})) {
            break :blk candidate;
        } else |_| {
            break :blk null;
        }
    };

    // Build file list for "include"
    var include_files: std.ArrayList([]const u8) = .empty;
    defer include_files.deinit(allocator);

    for (virtual_files) |vf| {
        const basename = std.fs.path.basename(vf.virtual_path);
        try include_files.append(allocator, basename);
    }

    // Create the tsconfig content
    const file = try tmp_dir.createFile("tsconfig.json", .{});
    defer file.close();

    try file.writeAll("{\n");

    // Extend project tsconfig if available (for path aliases)
    if (project_tsconfig) |ptsconfig| {
        // Convert to absolute path for reliable resolution
        const abs_path = try std.fs.cwd().realpathAlloc(allocator, ptsconfig);
        try file.writeAll("  \"extends\": \"");
        try file.writeAll(abs_path);
        try file.writeAll("\",\n");
    }

    // Compiler options for virtual files
    try file.writeAll("  \"compilerOptions\": {\n");
    try file.writeAll("    \"noEmit\": true,\n");
    try file.writeAll("    \"skipLibCheck\": true,\n");
    try file.writeAll("    \"allowJs\": true,\n");
    try file.writeAll("    \"checkJs\": true\n");
    try file.writeAll("  },\n");

    // Include only our virtual files
    try file.writeAll("  \"include\": [\n");
    for (include_files.items, 0..) |file_name, i| {
        try file.writeAll("    \"");
        try file.writeAll(file_name);
        try file.writeAll("\"");
        if (i < include_files.items.len - 1) {
            try file.writeAll(",");
        }
        try file.writeAll("\n");
    }
    try file.writeAll("  ]\n");
    try file.writeAll("}\n");

    return true;
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

test "writeTempTsconfig generates valid config" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp dir to test
    var tmp_dir = try std.fs.cwd().makeOpenPath(".svelte-check-zig-test-tmp", .{});
    defer {
        tmp_dir.close();
        std.fs.cwd().deleteTree(".svelte-check-zig-test-tmp") catch {};
    }

    const virtual_files = [_]VirtualFile{.{
        .original_path = "test.svelte",
        .virtual_path = "test.svelte.ts",
        .content = "const x = 1;",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    // Test with no project tsconfig (should still create config)
    const result = try writeTempTsconfig(allocator, tmp_dir, "nonexistent-dir", null, &virtual_files);
    try std.testing.expect(result);

    // Read generated config
    const content = try tmp_dir.readFileAlloc(allocator, "tsconfig.json", 10 * 1024);

    // Verify basic structure
    try std.testing.expect(std.mem.indexOf(u8, content, "\"compilerOptions\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "\"include\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "test.svelte.ts") != null);
    // No extends when project tsconfig doesn't exist
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\"") == null);
}

test "writeTempTsconfig extends project config when available" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a temp dir to test
    var tmp_dir = try std.fs.cwd().makeOpenPath(".svelte-check-zig-test-tmp", .{});
    defer {
        tmp_dir.close();
        std.fs.cwd().deleteTree(".svelte-check-zig-test-tmp") catch {};
    }

    const virtual_files = [_]VirtualFile{.{
        .original_path = "test.svelte",
        .virtual_path = "test.svelte.ts",
        .content = "const x = 1;",
        .source_map = .{ .mappings = &.{}, .svelte_source = "" },
    }};

    // Test with a real project tsconfig (using test-fixtures)
    const result = try writeTempTsconfig(allocator, tmp_dir, "test-fixtures/projects/sveltekit", null, &virtual_files);
    try std.testing.expect(result);

    // Read generated config
    const content = try tmp_dir.readFileAlloc(allocator, "tsconfig.json", 10 * 1024);

    // Verify extends is present (inherits path aliases)
    try std.testing.expect(std.mem.indexOf(u8, content, "\"extends\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "tsconfig.json") != null);
}
