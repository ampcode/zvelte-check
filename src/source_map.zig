//! Source mapping between Svelte and generated TypeScript
//!
//! Maps positions in virtual .ts files back to original .svelte files.

const std = @import("std");

pub const SourceMap = struct {
    mappings: []const Mapping,
    svelte_source: []const u8,

    pub const Mapping = struct {
        svelte_offset: u32,
        ts_offset: u32,
        len: u32,
    };

    pub fn tsToSvelte(self: *const SourceMap, ts_offset: u32) ?u32 {
        for (self.mappings) |m| {
            // Early exit: mappings are monotonic by ts_offset, so if we've
            // passed the target offset, no later mapping can match
            if (ts_offset < m.ts_offset) return null;
            if (ts_offset < m.ts_offset + m.len) {
                const delta = ts_offset - m.ts_offset;
                return m.svelte_offset + delta;
            }
        }
        return null;
    }

    pub fn svelteToTs(self: *const SourceMap, svelte_offset: u32) ?u32 {
        for (self.mappings) |m| {
            if (svelte_offset >= m.svelte_offset and svelte_offset < m.svelte_offset + m.len) {
                const delta = svelte_offset - m.svelte_offset;
                return m.ts_offset + delta;
            }
        }
        return null;
    }
};

pub const LineTable = struct {
    line_starts: []const u32,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !LineTable {
        var starts: std.ArrayList(u32) = .empty;
        try starts.append(allocator, 0);

        for (source, 0..) |c, i| {
            if (c == '\n' and i + 1 < source.len) {
                try starts.append(allocator, @intCast(i + 1));
            }
        }

        return .{ .line_starts = try starts.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *LineTable, allocator: std.mem.Allocator) void {
        allocator.free(self.line_starts);
    }

    pub fn offsetToLineCol(self: *const LineTable, offset: u32) struct { line: u32, col: u32 } {
        var line: u32 = 0;
        for (self.line_starts, 0..) |start, i| {
            if (start > offset) break;
            line = @intCast(i);
        }

        const col = offset - self.line_starts[line];
        return .{ .line = line, .col = col };
    }

    /// Convert 1-based line/col to byte offset. Returns null if out of bounds.
    pub fn lineColToOffset(self: *const LineTable, line: u32, col: u32) ?u32 {
        // tsgo uses 1-based line numbers
        if (line == 0) return null;
        const line_idx = line - 1;
        if (line_idx >= self.line_starts.len) return null;
        return self.line_starts[line_idx] + col - 1; // col is also 1-based
    }
};

test "line table" {
    const source = "line1\nline2\nline3";
    var table = try LineTable.init(std.testing.allocator, source);
    defer table.deinit(std.testing.allocator);

    const pos1 = table.offsetToLineCol(0);
    try std.testing.expectEqual(@as(u32, 0), pos1.line);
    try std.testing.expectEqual(@as(u32, 0), pos1.col);

    const pos2 = table.offsetToLineCol(6);
    try std.testing.expectEqual(@as(u32, 1), pos2.line);
    try std.testing.expectEqual(@as(u32, 0), pos2.col);
}

test "source map lookup" {
    const map: SourceMap = .{
        .mappings = &.{
            .{ .svelte_offset = 10, .ts_offset = 50, .len = 20 },
        },
        .svelte_source = "",
    };

    const svelte_pos = map.tsToSvelte(55);
    try std.testing.expectEqual(@as(?u32, 15), svelte_pos);

    const ts_pos = map.svelteToTs(15);
    try std.testing.expectEqual(@as(?u32, 55), ts_pos);
}
