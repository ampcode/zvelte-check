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

    /// Maps a TS offset to a Svelte offset, with fallback for unmapped regions.
    /// For errors in generated code (template bindings, type stubs), returns
    /// the nearest preceding mapped position. This prevents line numbers from
    /// extending beyond the Svelte file's actual length.
    pub fn tsToSvelteFallback(self: *const SourceMap, ts_offset: u32) u32 {
        var last_mapped_end: u32 = 0;

        for (self.mappings) |m| {
            // Exact match within a mapping range
            if (ts_offset >= m.ts_offset and ts_offset < m.ts_offset + m.len) {
                const delta = ts_offset - m.ts_offset;
                return m.svelte_offset + delta;
            }

            // Track the end of the last mapping we passed (for fallback)
            if (ts_offset >= m.ts_offset + m.len) {
                last_mapped_end = m.svelte_offset + m.len;
            }
        }

        // Fallback: return end of last mapping, or 0 if before all mappings
        return if (last_mapped_end > 0) last_mapped_end - 1 else 0;
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

test "source map fallback for unmapped regions" {
    // Simulates: type stubs at TS 0-50, then script at TS 50-70 maps to Svelte 10-30,
    // then generated template bindings at TS 70+ with no mapping
    const map: SourceMap = .{
        .mappings = &.{
            .{ .svelte_offset = 10, .ts_offset = 50, .len = 20 },
        },
        .svelte_source = "",
    };

    // Within mapping - exact match
    const exact = map.tsToSvelteFallback(55);
    try std.testing.expectEqual(@as(u32, 15), exact);

    // Before any mappings (in type stubs region) - falls back to 0
    const before = map.tsToSvelteFallback(25);
    try std.testing.expectEqual(@as(u32, 0), before);

    // After mapping ends (in generated template region) - falls back to end of last mapping
    const after = map.tsToSvelteFallback(100);
    try std.testing.expectEqual(@as(u32, 29), after); // 10 + 20 - 1 = 29
}
