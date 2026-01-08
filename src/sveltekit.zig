//! SvelteKit route detection and support
//!
//! Detects SvelteKit route files and provides type information:
//! - +page.svelte, +layout.svelte, +error.svelte
//! - Route variants: +page@.svelte, +page@group.svelte
//! - Maps ./$types imports to proper $types.d.ts paths

const std = @import("std");

pub const RouteKind = enum {
    page, // +page.svelte
    layout, // +layout.svelte
    @"error", // +error.svelte
    none, // Not a route file
};

pub const RouteInfo = struct {
    kind: RouteKind,
    layout_group: ?[]const u8, // e.g., "admin" from +page@admin.svelte
    route_dir: []const u8, // Directory containing the route file

    pub fn isRoute(self: RouteInfo) bool {
        return self.kind != .none;
    }

    /// Returns the data type name for this route kind
    pub fn dataTypeName(self: RouteInfo) ?[]const u8 {
        return switch (self.kind) {
            .page => "PageData",
            .layout => "LayoutData",
            .@"error" => null, // Error pages don't have data
            .none => null,
        };
    }

    /// Returns the load function type name
    pub fn loadTypeName(self: RouteInfo) ?[]const u8 {
        return switch (self.kind) {
            .page => "PageLoad",
            .layout => "LayoutLoad",
            .@"error", .none => null,
        };
    }

    /// Returns the server load type name
    pub fn serverLoadTypeName(self: RouteInfo) ?[]const u8 {
        return switch (self.kind) {
            .page => "PageServerLoad",
            .layout => "LayoutServerLoad",
            .@"error", .none => null,
        };
    }
};

/// Detects if a file path is a SvelteKit route file
pub fn detectRoute(file_path: []const u8) RouteInfo {
    const basename = std.fs.path.basename(file_path);
    const dir = std.fs.path.dirname(file_path) orelse ".";

    // Check for route patterns: +page.svelte, +layout.svelte, +error.svelte
    // With optional layout group: +page@.svelte, +page@admin.svelte

    if (!std.mem.startsWith(u8, basename, "+")) {
        return .{ .kind = .none, .layout_group = null, .route_dir = dir };
    }

    if (!std.mem.endsWith(u8, basename, ".svelte")) {
        return .{ .kind = .none, .layout_group = null, .route_dir = dir };
    }

    // Strip the .svelte extension and the + prefix
    const name_with_ext = basename[1..]; // Remove +
    const name = name_with_ext[0 .. name_with_ext.len - 7]; // Remove .svelte

    // Check for layout group: page@admin or page@
    var base_name: []const u8 = name;
    var layout_group: ?[]const u8 = null;

    if (std.mem.indexOf(u8, name, "@")) |at_pos| {
        base_name = name[0..at_pos];
        const group = name[at_pos + 1 ..];
        layout_group = if (group.len > 0) group else null;
    }

    const kind: RouteKind = if (std.mem.eql(u8, base_name, "page"))
        .page
    else if (std.mem.eql(u8, base_name, "layout"))
        .layout
    else if (std.mem.eql(u8, base_name, "error"))
        .@"error"
    else
        .none;

    return .{
        .kind = kind,
        .layout_group = layout_group,
        .route_dir = dir,
    };
}

/// Checks if a directory contains a load file (+page.ts, +page.server.ts, etc.)
pub fn hasLoadFile(route_dir: []const u8, kind: RouteKind) bool {
    const load_files = switch (kind) {
        .page => &[_][]const u8{ "+page.ts", "+page.js", "+page.server.ts", "+page.server.js" },
        .layout => &[_][]const u8{ "+layout.ts", "+layout.js", "+layout.server.ts", "+layout.server.js" },
        .@"error", .none => return false,
    };

    var dir = std.fs.cwd().openDir(route_dir, .{}) catch return false;
    defer dir.close();

    for (load_files) |file| {
        if (dir.access(file, .{})) |_| {
            return true;
        } else |_| {
            continue;
        }
    }

    return false;
}

/// Finds the .svelte-kit directory relative to a route file
pub fn findSvelteKitDir(allocator: std.mem.Allocator, route_path: []const u8) !?[]const u8 {
    var current = std.fs.path.dirname(route_path) orelse ".";

    // Walk up the directory tree looking for .svelte-kit
    var depth: u32 = 0;
    while (depth < 20) : (depth += 1) {
        const svelte_kit_path = try std.fs.path.join(allocator, &.{ current, ".svelte-kit" });

        var dir = std.fs.cwd().openDir(svelte_kit_path, .{}) catch |err| {
            allocator.free(svelte_kit_path);
            if (err == error.FileNotFound) {
                const parent = std.fs.path.dirname(current);
                if (parent == null or std.mem.eql(u8, parent.?, current)) {
                    return null;
                }
                current = parent.?;
                continue;
            }
            return err;
        };
        dir.close();
        return svelte_kit_path;
    }

    return null;
}

/// Computes the relative path from routes to the $types.d.ts file
/// e.g., for "src/routes/admin/+page.svelte" returns "src/routes/admin"
pub fn getTypesRelativePath(allocator: std.mem.Allocator, route_dir: []const u8) ![]const u8 {
    // The $types.d.ts file is in .svelte-kit/types/{route_dir}/$types.d.ts
    // Route files import from "./$types" which needs to resolve correctly
    _ = allocator;
    return route_dir;
}

// Tests

test "detect +page.svelte" {
    const info = detectRoute("src/routes/+page.svelte");
    try std.testing.expectEqual(RouteKind.page, info.kind);
    try std.testing.expect(info.layout_group == null);
    try std.testing.expectEqualStrings("PageData", info.dataTypeName().?);
}

test "detect +layout.svelte" {
    const info = detectRoute("src/routes/+layout.svelte");
    try std.testing.expectEqual(RouteKind.layout, info.kind);
    try std.testing.expect(info.layout_group == null);
    try std.testing.expectEqualStrings("LayoutData", info.dataTypeName().?);
}

test "detect +error.svelte" {
    const info = detectRoute("src/routes/+error.svelte");
    try std.testing.expectEqual(RouteKind.@"error", info.kind);
    try std.testing.expect(info.dataTypeName() == null);
}

test "detect +page@.svelte (root layout reset)" {
    const info = detectRoute("src/routes/admin/+page@.svelte");
    try std.testing.expectEqual(RouteKind.page, info.kind);
    try std.testing.expect(info.layout_group == null);
}

test "detect +page@admin.svelte (layout group)" {
    const info = detectRoute("src/routes/admin/users/+page@admin.svelte");
    try std.testing.expectEqual(RouteKind.page, info.kind);
    try std.testing.expectEqualStrings("admin", info.layout_group.?);
}

test "detect +layout@group.svelte" {
    const info = detectRoute("src/routes/(marketing)/+layout@.svelte");
    try std.testing.expectEqual(RouteKind.layout, info.kind);
    try std.testing.expect(info.layout_group == null);
}

test "non-route file returns none" {
    const info = detectRoute("src/lib/Button.svelte");
    try std.testing.expectEqual(RouteKind.none, info.kind);
}

test "file without + prefix" {
    const info = detectRoute("src/routes/page.svelte");
    try std.testing.expectEqual(RouteKind.none, info.kind);
}

test "nested route detection" {
    const info = detectRoute("src/routes/blog/[slug]/+page.svelte");
    try std.testing.expectEqual(RouteKind.page, info.kind);
    try std.testing.expectEqualStrings("src/routes/blog/[slug]", info.route_dir);
}
