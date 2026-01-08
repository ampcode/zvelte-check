//! Workspace scanning - finds all .svelte files

const std = @import("std");

pub fn scan(
    allocator: std.mem.Allocator,
    workspace_path: []const u8,
    ignore_patterns: []const []const u8,
) ![]const []const u8 {
    var files: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (files.items) |f| allocator.free(f);
        files.deinit(allocator);
    }

    var dir = std.fs.cwd().openDir(workspace_path, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) return files.toOwnedSlice(allocator);
        return err;
    };
    defer dir.close();

    try walkDir(allocator, dir, workspace_path, ignore_patterns, &files);

    return files.toOwnedSlice(allocator);
}

fn walkDir(
    allocator: std.mem.Allocator,
    dir: std.fs.Dir,
    base_path: []const u8,
    ignore_patterns: []const []const u8,
    files: *std.ArrayList([]const u8),
) !void {
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (shouldIgnore(entry.name, ignore_patterns)) continue;

        const full_path = try std.fs.path.join(allocator, &.{ base_path, entry.name });
        errdefer allocator.free(full_path);

        switch (entry.kind) {
            .directory => {
                if (std.mem.eql(u8, entry.name, "node_modules")) continue;
                if (std.mem.eql(u8, entry.name, ".git")) continue;
                if (std.mem.eql(u8, entry.name, ".svelte-kit")) continue;
                if (std.mem.eql(u8, entry.name, "build")) continue;
                if (std.mem.eql(u8, entry.name, "dist")) continue;

                var sub_dir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer sub_dir.close();
                try walkDir(allocator, sub_dir, full_path, ignore_patterns, files);
                allocator.free(full_path);
            },
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".svelte")) {
                    try files.append(allocator, full_path);
                } else {
                    allocator.free(full_path);
                }
            },
            else => allocator.free(full_path),
        }
    }
}

fn shouldIgnore(name: []const u8, patterns: []const []const u8) bool {
    for (patterns) |pattern| {
        if (matchGlob(pattern, name)) return true;
    }
    return false;
}

fn matchGlob(pattern: []const u8, name: []const u8) bool {
    // Simple glob matching: just check if pattern is substring or exact match
    // TODO: implement proper glob matching
    if (std.mem.eql(u8, pattern, name)) return true;
    if (std.mem.indexOf(u8, name, pattern)) |_| return true;
    return false;
}

test "scan empty dir" {
    // Smoke test
    const allocator = std.testing.allocator;
    const files = try scan(allocator, ".", &.{});
    defer {
        for (files) |f| allocator.free(f);
        allocator.free(files);
    }
}
