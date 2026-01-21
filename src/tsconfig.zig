//! tsconfig.json parser for file filtering
//!
//! Parses tsconfig.json to extract include/exclude patterns,
//! handles extends chains, and filters file paths accordingly.

const std = @import("std");

pub const TsConfig = struct {
    include: []const []const u8,
    exclude: []const []const u8,
    /// Paths to referenced projects (from "references" field)
    references: []const []const u8 = &.{},

    pub fn deinit(self: *TsConfig, allocator: std.mem.Allocator) void {
        for (self.include) |p| allocator.free(p);
        allocator.free(self.include);
        for (self.exclude) |p| allocator.free(p);
        allocator.free(self.exclude);
        for (self.references) |p| allocator.free(p);
        allocator.free(self.references);
    }
};

/// Loads and parses tsconfig.json from workspace, handling extends chains.
/// Returns null if no tsconfig found or if parsing fails.
pub fn load(allocator: std.mem.Allocator, workspace_path: []const u8, tsconfig_path: ?[]const u8) !?TsConfig {
    const config_name = tsconfig_path orelse "tsconfig.json";

    const full_path = try std.fs.path.join(allocator, &.{ workspace_path, config_name });
    defer allocator.free(full_path);

    return loadFromPath(allocator, full_path, workspace_path);
}

const LoadError = std.fs.File.OpenError || std.fs.File.ReadError || std.mem.Allocator.Error;

fn loadFromPath(allocator: std.mem.Allocator, config_path: []const u8, base_dir: []const u8) LoadError!?TsConfig {
    const content = std.fs.cwd().readFileAlloc(allocator, config_path, 1024 * 1024) catch |err| {
        if (err == error.FileNotFound) return null;
        return err;
    };
    defer allocator.free(content);

    return parseConfig(allocator, content, base_dir);
}

fn parseConfig(allocator: std.mem.Allocator, content: []const u8, base_dir: []const u8) !?TsConfig {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch {
        return null;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return null;

    var include: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (include.items) |p| allocator.free(p);
        include.deinit(allocator);
    }
    var exclude: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (exclude.items) |p| allocator.free(p);
        exclude.deinit(allocator);
    }
    var references: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (references.items) |p| allocator.free(p);
        references.deinit(allocator);
    }

    // Handle extends first (base config patterns come before current)
    if (root.object.get("extends")) |extends_val| {
        const extends_path = switch (extends_val) {
            .string => |s| s,
            else => null,
        };
        if (extends_path) |ext| {
            // Resolve extends path relative to current config's directory
            const resolved = try resolveExtendsPath(allocator, ext, base_dir);
            defer allocator.free(resolved);

            const parent_dir = std.fs.path.dirname(resolved) orelse base_dir;
            if (try loadFromPath(allocator, resolved, parent_dir)) |parent| {
                var parent_mut = parent;
                defer parent_mut.deinit(allocator);

                // Inherit patterns from parent (will be overridden if current config has them)
                for (parent_mut.include) |p| {
                    try include.append(allocator, try allocator.dupe(u8, p));
                }
                for (parent_mut.exclude) |p| {
                    try exclude.append(allocator, try allocator.dupe(u8, p));
                }
            }
        }
    }

    // Parse include array (overrides inherited if present)
    if (root.object.get("include")) |include_val| {
        // Clear inherited includes if current config specifies its own
        for (include.items) |p| allocator.free(p);
        include.clearRetainingCapacity();

        if (include_val == .array) {
            for (include_val.array.items) |item| {
                if (item == .string) {
                    try include.append(allocator, try allocator.dupe(u8, item.string));
                }
            }
        }
    }

    // Parse exclude array (overrides inherited if present)
    if (root.object.get("exclude")) |exclude_val| {
        // Clear inherited excludes if current config specifies its own
        for (exclude.items) |p| allocator.free(p);
        exclude.clearRetainingCapacity();

        if (exclude_val == .array) {
            for (exclude_val.array.items) |item| {
                if (item == .string) {
                    try exclude.append(allocator, try allocator.dupe(u8, item.string));
                }
            }
        }
    }

    // Parse references array - these are paths to other tsconfig projects
    // Format: [{"path": "../lib/web-ui"}, {"path": "../core"}]
    if (root.object.get("references")) |refs_val| {
        if (refs_val == .array) {
            for (refs_val.array.items) |item| {
                if (item == .object) {
                    if (item.object.get("path")) |path_val| {
                        if (path_val == .string) {
                            // Resolve the reference path relative to base_dir
                            const ref_path = try std.fs.path.join(allocator, &.{ base_dir, path_val.string });
                            try references.append(allocator, ref_path);
                        }
                    }
                }
            }
        }
    }

    return .{
        .include = try include.toOwnedSlice(allocator),
        .exclude = try exclude.toOwnedSlice(allocator),
        .references = try references.toOwnedSlice(allocator),
    };
}

/// Resolves an extends path (handles node_modules packages and relative paths)
fn resolveExtendsPath(allocator: std.mem.Allocator, extends: []const u8, base_dir: []const u8) ![]const u8 {
    // Relative path (starts with . or /)
    if (extends.len > 0 and (extends[0] == '.' or extends[0] == '/')) {
        var path = try std.fs.path.join(allocator, &.{ base_dir, extends });

        // If no extension, try adding .json
        if (std.fs.path.extension(extends).len == 0) {
            const with_json = try std.mem.concat(allocator, u8, &.{ path, ".json" });
            allocator.free(path);
            path = with_json;
        }
        return path;
    }

    // Package reference (e.g., "@sveltejs/kit/tsconfig.json")
    // Look in node_modules
    const node_modules_path = try std.fs.path.join(allocator, &.{ base_dir, "node_modules", extends });
    defer allocator.free(node_modules_path);

    // Check if it exists directly
    if (std.fs.cwd().access(node_modules_path, .{})) {
        return try allocator.dupe(u8, node_modules_path);
    } else |_| {}

    // Try package.json lookup for the package's tsconfig
    // For @scope/package, we need the package directory
    const package_name = if (std.mem.indexOf(u8, extends, "/")) |slash| blk: {
        // If starts with @, include second component
        if (extends[0] == '@') {
            if (std.mem.indexOfPos(u8, extends, slash + 1, "/")) |second_slash| {
                break :blk extends[0..second_slash];
            }
        }
        break :blk extends[0..slash];
    } else extends;

    const package_dir = try std.fs.path.join(allocator, &.{ base_dir, "node_modules", package_name });
    defer allocator.free(package_dir);

    // Look for package.json to find tsconfig path
    const package_json = try std.fs.path.join(allocator, &.{ package_dir, "package.json" });
    defer allocator.free(package_json);

    if (std.fs.cwd().readFileAlloc(allocator, package_json, 64 * 1024)) |pkg_content| {
        defer allocator.free(pkg_content);

        if (std.json.parseFromSlice(std.json.Value, allocator, pkg_content, .{})) |pkg_parsed| {
            defer pkg_parsed.deinit();

            if (pkg_parsed.value.object.get("tsconfig")) |tsconfig_val| {
                if (tsconfig_val == .string) {
                    return try std.fs.path.join(allocator, &.{ package_dir, tsconfig_val.string });
                }
            }
        } else |_| {}
    } else |_| {}

    // Default: assume extends points to a file directly
    return try std.fs.path.join(allocator, &.{ base_dir, "node_modules", extends });
}

/// Checks if a file path matches the tsconfig include/exclude patterns.
/// Returns true if the file should be included for checking.
pub fn shouldInclude(config: TsConfig, file_path: []const u8) bool {
    // Empty include means no files match
    if (config.include.len == 0) return false;

    // Check if file matches any include pattern
    var included = false;
    for (config.include) |pattern| {
        if (matchTsconfigPattern(pattern, file_path)) {
            included = true;
            break;
        }
    }

    if (!included) return false;

    // Check if file matches any exclude pattern
    for (config.exclude) |pattern| {
        if (matchTsconfigPattern(pattern, file_path)) {
            return false;
        }
    }

    return true;
}

/// Matches a tsconfig pattern, handling the TypeScript convention where
/// a bare directory name like "src" means "src/**/*"
fn matchTsconfigPattern(pattern: []const u8, path: []const u8) bool {
    // If pattern has no wildcards or extensions, treat it as a directory
    // TypeScript convention: "src" means "src/**/*"
    const has_wildcard = std.mem.indexOf(u8, pattern, "*") != null;
    const has_extension = std.mem.indexOf(u8, pattern, ".") != null;

    if (!has_wildcard and !has_extension) {
        // It's a directory pattern - check if path starts with pattern/
        if (std.mem.startsWith(u8, path, pattern)) {
            // Ensure it's a directory match (path has "/" after pattern)
            if (path.len > pattern.len and path[pattern.len] == '/') {
                return true;
            }
        }
        return false;
    }

    return matchGlob(pattern, path);
}

/// Matches a tsconfig glob pattern against a file path.
/// Supports:
/// - ** for any directory depth
/// - * for any characters within a path segment
/// - ? for single character
fn matchGlob(pattern: []const u8, path: []const u8) bool {
    return matchGlobInner(pattern, path);
}

fn matchGlobInner(pattern: []const u8, path: []const u8) bool {
    var p_idx: usize = 0;
    var s_idx: usize = 0;

    // Track ** positions for backtracking
    var star_p_idx: ?usize = null;
    var star_s_idx: usize = 0;

    while (s_idx < path.len) {
        if (p_idx < pattern.len) {
            const p_char = pattern[p_idx];

            // Handle **
            if (p_idx + 1 < pattern.len and pattern[p_idx] == '*' and pattern[p_idx + 1] == '*') {
                // Skip the **
                p_idx += 2;
                // Skip optional trailing /
                if (p_idx < pattern.len and pattern[p_idx] == '/') {
                    p_idx += 1;
                }
                // Remember position for backtracking
                star_p_idx = p_idx;
                star_s_idx = s_idx;
                continue;
            }

            // Handle single *
            if (p_char == '*') {
                p_idx += 1;
                // * matches anything except /
                while (s_idx < path.len and path[s_idx] != '/') {
                    // Try matching rest
                    if (matchGlobInner(pattern[p_idx..], path[s_idx..])) {
                        return true;
                    }
                    s_idx += 1;
                }
                continue;
            }

            // Handle ?
            if (p_char == '?') {
                if (path[s_idx] != '/') {
                    p_idx += 1;
                    s_idx += 1;
                    continue;
                }
            }

            // Exact character match
            if (p_char == path[s_idx]) {
                p_idx += 1;
                s_idx += 1;
                continue;
            }
        }

        // Mismatch - try backtracking to **
        if (star_p_idx) |sp| {
            star_s_idx += 1;
            s_idx = star_s_idx;
            p_idx = sp;
            continue;
        }

        return false;
    }

    // Check remaining pattern
    while (p_idx < pattern.len) {
        if (pattern[p_idx] == '*') {
            p_idx += 1;
        } else if (p_idx + 1 < pattern.len and pattern[p_idx] == '*' and pattern[p_idx + 1] == '*') {
            p_idx += 2;
            if (p_idx < pattern.len and pattern[p_idx] == '/') {
                p_idx += 1;
            }
        } else {
            break;
        }
    }

    return p_idx == pattern.len;
}

// Tests

test "matchGlob exact match" {
    try std.testing.expect(matchGlob("src/app.ts", "src/app.ts"));
    try std.testing.expect(!matchGlob("src/app.ts", "src/app.js"));
}

test "matchGlob single star" {
    try std.testing.expect(matchGlob("src/*.ts", "src/app.ts"));
    try std.testing.expect(matchGlob("src/*.ts", "src/index.ts"));
    try std.testing.expect(!matchGlob("src/*.ts", "src/sub/app.ts"));
    try std.testing.expect(!matchGlob("src/*.ts", "lib/app.ts"));
}

test "matchGlob double star" {
    try std.testing.expect(matchGlob("src/**/*.ts", "src/app.ts"));
    try std.testing.expect(matchGlob("src/**/*.ts", "src/sub/app.ts"));
    try std.testing.expect(matchGlob("src/**/*.ts", "src/a/b/c/app.ts"));
    try std.testing.expect(!matchGlob("src/**/*.ts", "lib/app.ts"));
}

test "matchGlob double star at end" {
    try std.testing.expect(matchGlob("src/**", "src/app.ts"));
    try std.testing.expect(matchGlob("src/**", "src/sub/app.ts"));
    try std.testing.expect(matchGlob("node_modules/**", "node_modules/svelte/index.js"));
}

test "matchGlob leading double star" {
    try std.testing.expect(matchGlob("**/*.svelte", "src/app.svelte"));
    try std.testing.expect(matchGlob("**/*.svelte", "components/Button.svelte"));
    try std.testing.expect(!matchGlob("**/*.svelte", "src/app.ts"));
}

test "matchGlob svelte patterns" {
    // Common tsconfig patterns for svelte projects
    try std.testing.expect(matchGlob("src/**/*.svelte", "src/routes/+page.svelte"));
    try std.testing.expect(matchGlob("src/**/*.ts", "src/lib/utils.ts"));
    try std.testing.expect(!matchGlob("src/**/*.ts", "node_modules/svelte/index.js"));
}

test "shouldInclude basic" {
    const config: TsConfig = .{
        .include = &.{"src/**/*.svelte"},
        .exclude = &.{"src/test/**"},
    };

    try std.testing.expect(shouldInclude(config, "src/app.svelte"));
    try std.testing.expect(shouldInclude(config, "src/routes/+page.svelte"));
    try std.testing.expect(!shouldInclude(config, "lib/app.svelte"));
    try std.testing.expect(!shouldInclude(config, "src/test/app.svelte"));
}

test "shouldInclude empty include" {
    const config: TsConfig = .{
        .include = &.{},
        .exclude = &.{},
    };

    try std.testing.expect(!shouldInclude(config, "src/app.svelte"));
}

test "parseConfig basic" {
    const allocator = std.testing.allocator;

    const json =
        \\{
        \\  "include": ["src/**/*.ts", "src/**/*.svelte"],
        \\  "exclude": ["node_modules"]
        \\}
    ;

    var config = (try parseConfig(allocator, json, ".")).?;
    defer config.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), config.include.len);
    try std.testing.expectEqualStrings("src/**/*.ts", config.include[0]);
    try std.testing.expectEqualStrings("src/**/*.svelte", config.include[1]);
    try std.testing.expectEqual(@as(usize, 1), config.exclude.len);
    try std.testing.expectEqualStrings("node_modules", config.exclude[0]);
}

test "parseConfig empty" {
    const allocator = std.testing.allocator;

    const json = "{}";

    var config = (try parseConfig(allocator, json, ".")).?;
    defer config.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), config.include.len);
    try std.testing.expectEqual(@as(usize, 0), config.exclude.len);
}

test "matchTsconfigPattern directory pattern" {
    // "src" should match "src/foo.ts" but not "source/foo.ts"
    try std.testing.expect(matchTsconfigPattern("src", "src/foo.ts"));
    try std.testing.expect(matchTsconfigPattern("src", "src/components/foo.svelte"));
    try std.testing.expect(!matchTsconfigPattern("src", "source/foo.ts"));
    try std.testing.expect(!matchTsconfigPattern("src", "lib/src/foo.ts"));
}

test "matchTsconfigPattern with extension" {
    // Patterns with extensions are file patterns, not directory patterns
    try std.testing.expect(matchTsconfigPattern("vite.config.ts", "vite.config.ts"));
    try std.testing.expect(!matchTsconfigPattern("vite.config.ts", "src/vite.config.ts"));
}
