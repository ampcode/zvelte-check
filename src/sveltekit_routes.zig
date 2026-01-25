//! SvelteKit route file type injection
//!
//! Transforms SvelteKit route files (+page.ts, +page.server.ts, etc.) to inject types
//! for untyped exports. This matches svelte2tsx's upsertKitRouteFile() behavior.
//!
//! For example, transforms:
//!   export async function load({ locals, url }) { ... }
//! Into:
//!   export async function load({ locals, url }: import('./$types.js').PageServerLoadEvent) { ... }
//!
//! And transforms:
//!   export const load = async ({ data }) => { ... }
//! Into:
//!   export const load = (async ({ data }) => { ... }) satisfies import('./$types.js').PageLoad

const std = @import("std");

/// Route file type (determines which types to inject)
pub const RouteFileKind = enum {
    page, // +page.ts (universal load)
    page_server, // +page.server.ts (server load)
    layout, // +layout.ts (universal layout load)
    layout_server, // +layout.server.ts (server layout load)
    server, // +server.ts (API endpoint)
    none, // Not a route file
};

/// Represents an injection point in the source
const Injection = struct {
    pos: usize,
    text: []const u8,
};

/// Transforms a SvelteKit route file by injecting types for untyped exports.
/// Returns the transformed content, or null if no transformation needed.
pub fn transformRouteFile(
    allocator: std.mem.Allocator,
    source: []const u8,
    file_path: []const u8,
) !?[]const u8 {
    const kind = getRouteFileKind(file_path);
    if (kind == .none) return null;

    // Find all injection points
    var injections: std.ArrayList(Injection) = .empty;
    defer injections.deinit(allocator);

    try findInjections(allocator, source, kind, &injections);

    if (injections.items.len == 0) return null;

    // Sort injections by position (descending) so we can apply them from end to start
    std.mem.sort(Injection, injections.items, {}, struct {
        fn lessThan(_: void, a: Injection, b: Injection) bool {
            return a.pos > b.pos; // Descending
        }
    }.lessThan);

    // Build output by applying injections
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);

    try output.appendSlice(allocator, source);

    for (injections.items) |inj| {
        // Insert at position
        try output.insertSlice(allocator, inj.pos, inj.text);
    }

    return try output.toOwnedSlice(allocator);
}

/// Determines the route file kind from a file path
pub fn getRouteFileKind(path: []const u8) RouteFileKind {
    const filename = if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx|
        path[idx + 1 ..]
    else if (std.mem.lastIndexOfScalar(u8, path, '\\')) |idx|
        path[idx + 1 ..]
    else
        path;

    if (std.mem.eql(u8, filename, "+page.server.ts")) return .page_server;
    if (std.mem.eql(u8, filename, "+page.ts")) return .page;
    if (std.mem.eql(u8, filename, "+layout.server.ts")) return .layout_server;
    if (std.mem.eql(u8, filename, "+layout.ts")) return .layout;
    if (std.mem.eql(u8, filename, "+server.ts")) return .server;
    return .none;
}

/// Returns the event type annotation for a load function
fn getLoadEventType(kind: RouteFileKind) ?[]const u8 {
    return switch (kind) {
        .page => ": import('./$types.js').PageLoadEvent",
        .page_server => ": import('./$types.js').PageServerLoadEvent",
        .layout => ": import('./$types.js').LayoutLoadEvent",
        .layout_server => ": import('./$types.js').LayoutServerLoadEvent",
        .server, .none => null,
    };
}

/// Returns the satisfies type for const load declarations (with closing paren)
fn getLoadSatisfiesType(kind: RouteFileKind) ?[]const u8 {
    return switch (kind) {
        .page => ") satisfies import('./$types.js').PageLoad",
        .page_server => ") satisfies import('./$types.js').PageServerLoad",
        .layout => ") satisfies import('./$types.js').LayoutLoad",
        .layout_server => ") satisfies import('./$types.js').LayoutServerLoad",
        .server, .none => null,
    };
}

/// Find all injection points in the source
fn findInjections(
    allocator: std.mem.Allocator,
    source: []const u8,
    kind: RouteFileKind,
    injections: *std.ArrayList(Injection),
) !void {
    // Find load function injections
    try findLoadFunctionInjections(allocator, source, kind, injections);
    try findLoadConstInjections(allocator, source, kind, injections);

    // For +server.ts, find HTTP handler injections
    if (kind == .server) {
        for ([_][]const u8{ "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD" }) |method| {
            try findHttpHandlerInjections(allocator, source, method, injections);
        }
    }
}

/// Find injection points for `export [async] function load({ ... })` patterns
fn findLoadFunctionInjections(
    allocator: std.mem.Allocator,
    source: []const u8,
    kind: RouteFileKind,
    injections: *std.ArrayList(Injection),
) !void {
    const event_type = getLoadEventType(kind) orelse return;

    // Look for patterns: "export async function load(" or "export function load("
    const patterns = [_][]const u8{
        "export async function load(",
        "export function load(",
    };

    for (patterns) |pattern| {
        var search_pos: usize = 0;
        while (std.mem.indexOfPos(u8, source, search_pos, pattern)) |pos| {
            search_pos = pos + pattern.len;

            // Find the opening { of destructuring
            const after_paren = source[search_pos..];
            const trimmed = std.mem.trimLeft(u8, after_paren, " \t\n");

            if (trimmed.len == 0 or trimmed[0] != '{') continue;

            const brace_start = @intFromPtr(trimmed.ptr) - @intFromPtr(source.ptr);
            const close_brace = findMatchingBrace(source, brace_start) orelse continue;

            // Check if already has type annotation
            const after_brace = source[close_brace + 1 ..];
            const after_trimmed = std.mem.trimLeft(u8, after_brace, " \t\n");
            if (after_trimmed.len > 0 and after_trimmed[0] == ':') continue; // Already typed

            // Find closing ) - inject right before it
            if (std.mem.indexOfPos(u8, source, close_brace + 1, ")")) |close_paren| {
                // Inject type annotation at the position of close paren
                try injections.append(allocator, .{
                    .pos = close_paren,
                    .text = event_type,
                });
            }
        }
    }
}

/// Find injection points for `export const load = ...` patterns
fn findLoadConstInjections(
    allocator: std.mem.Allocator,
    source: []const u8,
    kind: RouteFileKind,
    injections: *std.ArrayList(Injection),
) !void {
    const satisfies_type = getLoadSatisfiesType(kind) orelse return;

    // Pattern: export const load = async ({ ... }) => ...
    // Or: export const load = ({ ... }) => ...
    // Or: export const load = async function({ ... }) { ... }

    var search_pos: usize = 0;
    while (std.mem.indexOfPos(u8, source, search_pos, "export const load")) |pos| {
        search_pos = pos + 17; // len("export const load")

        // Check if already has satisfies
        const line_end = findStatementEnd(source, pos) orelse continue;
        const statement = source[pos..line_end];
        if (std.mem.indexOf(u8, statement, "satisfies") != null) continue;

        // Check if it has a type annotation on the const
        const after_load = source[search_pos..];
        const trimmed = std.mem.trimLeft(u8, after_load, " \t");
        if (trimmed.len > 0 and trimmed[0] == ':') continue; // Already has type annotation

        // Find end of the function expression (this is tricky for arrow functions)
        // For now, look for common patterns and inject satisfies at statement end

        // Inject satisfies at end of statement (before any trailing semicolon/newline)
        var inject_pos = line_end;
        while (inject_pos > pos and (source[inject_pos - 1] == ';' or source[inject_pos - 1] == '\n' or source[inject_pos - 1] == ' ')) {
            inject_pos -= 1;
        }

        // We need to wrap the value in parens for satisfies to work with arrow functions
        // Find the = sign after "export const load"
        if (std.mem.indexOfPos(u8, source, search_pos, "=")) |eq_pos| {
            const after_eq = eq_pos + 1;
            // Insert ( after =
            try injections.append(allocator, .{
                .pos = after_eq,
                .text = "(",
            });
            // Insert ) satisfies Type at end
            try injections.append(allocator, .{
                .pos = inject_pos,
                .text = satisfies_type,
            });
        }
    }
}

/// Find injection points for HTTP handlers in +server.ts
fn findHttpHandlerInjections(
    allocator: std.mem.Allocator,
    source: []const u8,
    method: []const u8,
    injections: *std.ArrayList(Injection),
) !void {
    var fn_pattern_buf: [64]u8 = undefined;
    const fn_pattern = std.fmt.bufPrint(&fn_pattern_buf, "export async function {s}(", .{method}) catch return;
    var fn_pattern2_buf: [64]u8 = undefined;
    const fn_pattern2 = std.fmt.bufPrint(&fn_pattern2_buf, "export function {s}(", .{method}) catch return;

    // Check function form
    for ([_][]const u8{ fn_pattern, fn_pattern2 }) |pattern| {
        var search_pos: usize = 0;
        while (std.mem.indexOfPos(u8, source, search_pos, pattern)) |pos| {
            search_pos = pos + pattern.len;

            const after_paren = source[search_pos..];
            const trimmed = std.mem.trimLeft(u8, after_paren, " \t\n");

            if (trimmed.len == 0 or trimmed[0] != '{') continue;

            const brace_start = @intFromPtr(trimmed.ptr) - @intFromPtr(source.ptr);
            const close_brace = findMatchingBrace(source, brace_start) orelse continue;

            // Check if already has type annotation
            const after_brace = source[close_brace + 1 ..];
            const after_trimmed = std.mem.trimLeft(u8, after_brace, " \t\n");
            if (after_trimmed.len > 0 and after_trimmed[0] == ':') continue;

            // Inject type at close paren
            if (std.mem.indexOfPos(u8, source, close_brace + 1, ")")) |close_paren| {
                try injections.append(allocator, .{
                    .pos = close_paren,
                    .text = ": import('./$types.js').RequestEvent",
                });
            }
        }
    }

    // Check const form: export const GET = ...
    var const_pattern_buf: [64]u8 = undefined;
    const const_pattern = std.fmt.bufPrint(&const_pattern_buf, "export const {s}", .{method}) catch return;

    var search_pos: usize = 0;
    while (std.mem.indexOfPos(u8, source, search_pos, const_pattern)) |pos| {
        search_pos = pos + const_pattern.len;

        // Check if already has type annotation
        const after_name = source[search_pos..];
        const trimmed = std.mem.trimLeft(u8, after_name, " \t");
        if (trimmed.len > 0 and trimmed[0] == ':') continue; // Already has : RequestHandler

        // Check for = sign
        if (!std.mem.startsWith(u8, trimmed, "=")) continue;

        // Find statement end
        const line_end = findStatementEnd(source, pos) orelse continue;
        const statement = source[pos..line_end];
        if (std.mem.indexOf(u8, statement, "satisfies") != null) continue;

        // Insert type annotation after the name
        const type_pos = search_pos;
        try injections.append(allocator, .{
            .pos = type_pos,
            .text = ": import('./$types.js').RequestHandler",
        });
    }
}

/// Find the end of a statement (handles multi-line arrow functions)
/// This looks for the next export or the end of file, accounting for brace matching.
fn findStatementEnd(source: []const u8, start: usize) ?usize {
    // Find the = sign
    const eq_pos = std.mem.indexOfPos(u8, source, start, "=") orelse return null;

    // After =, we need to find where this statement ends
    // For arrow functions like `async ({ data }) => { ... }`, we need to match braces
    var i = eq_pos + 1;
    var brace_depth: i32 = 0;
    var paren_depth: i32 = 0;

    while (i < source.len) : (i += 1) {
        switch (source[i]) {
            '{' => brace_depth += 1,
            '}' => {
                brace_depth -= 1;
                // If we close the function body, that's the end
                if (brace_depth == 0 and paren_depth == 0) {
                    return i + 1;
                }
            },
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            '"', '\'' => {
                // Skip string
                const quote = source[i];
                i += 1;
                while (i < source.len and source[i] != quote) : (i += 1) {
                    if (source[i] == '\\' and i + 1 < source.len) i += 1;
                }
            },
            '`' => {
                // Skip template literal
                i += 1;
                while (i < source.len and source[i] != '`') : (i += 1) {
                    if (source[i] == '\\' and i + 1 < source.len) i += 1;
                    if (source[i] == '$' and i + 1 < source.len and source[i + 1] == '{') {
                        // Skip ${...} in template
                        i += 2;
                        var inner_depth: i32 = 1;
                        while (i < source.len and inner_depth > 0) : (i += 1) {
                            if (source[i] == '{') inner_depth += 1;
                            if (source[i] == '}') inner_depth -= 1;
                        }
                    }
                }
            },
            '\n' => {
                // If we're at depth 0 and hit newline, could be end of single-line arrow
                if (brace_depth == 0 and paren_depth == 0) {
                    // Check if next non-whitespace is 'export' or end
                    const remaining = source[i + 1 ..];
                    const trimmed = std.mem.trimLeft(u8, remaining, " \t\n");
                    if (trimmed.len == 0 or std.mem.startsWith(u8, trimmed, "export ")) {
                        return i;
                    }
                }
            },
            else => {},
        }
    }

    return source.len;
}

/// Find the matching closing brace for an opening brace at the given position
fn findMatchingBrace(source: []const u8, pos: usize) ?usize {
    if (pos >= source.len or source[pos] != '{') return null;

    var depth: u32 = 1;
    var i = pos + 1;
    while (i < source.len) : (i += 1) {
        switch (source[i]) {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if (depth == 0) return i;
            },
            '"', '\'' => {
                // Skip string literals
                const quote = source[i];
                i += 1;
                while (i < source.len and source[i] != quote) : (i += 1) {
                    if (source[i] == '\\' and i + 1 < source.len) i += 1;
                }
            },
            else => {},
        }
    }
    return null;
}

// Tests

test "getRouteFileKind" {
    try std.testing.expectEqual(RouteFileKind.page_server, getRouteFileKind("src/routes/+page.server.ts"));
    try std.testing.expectEqual(RouteFileKind.page, getRouteFileKind("src/routes/+page.ts"));
    try std.testing.expectEqual(RouteFileKind.layout_server, getRouteFileKind("src/routes/+layout.server.ts"));
    try std.testing.expectEqual(RouteFileKind.layout, getRouteFileKind("src/routes/+layout.ts"));
    try std.testing.expectEqual(RouteFileKind.server, getRouteFileKind("src/routes/api/+server.ts"));
    try std.testing.expectEqual(RouteFileKind.none, getRouteFileKind("src/lib/utils.ts"));
}

test "transformRouteFile injects type for load function" {
    const allocator = std.testing.allocator;
    const source =
        \\export async function load({ locals }) {
        \\  return {}
        \\}
    ;

    const result = try transformRouteFile(allocator, source, "+page.server.ts");
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?, "PageServerLoadEvent") != null);
}

test "transformRouteFile skips already typed functions" {
    const allocator = std.testing.allocator;
    const source =
        \\export async function load({ locals }: import('./$types').PageServerLoadEvent) {
        \\  return {}
        \\}
    ;

    const result = try transformRouteFile(allocator, source, "+page.server.ts");
    try std.testing.expect(result == null);
}

test "transformRouteFile injects type for const load" {
    const allocator = std.testing.allocator;
    const source =
        \\export const load = async ({ data }) => {
        \\  return { ...data }
        \\}
    ;

    const result = try transformRouteFile(allocator, source, "+page.ts");
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?, "satisfies") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?, "PageLoad") != null);
}

test "transformRouteFile handles multi-line const load" {
    const allocator = std.testing.allocator;
    const source =
        \\export const load = async ({ data }) => {
        \\        return {
        \\                ...data,
        \\                title: 'Transactional Emails',
        \\        }
        \\}
    ;

    const result = try transformRouteFile(allocator, source, "+page.ts");
    defer if (result) |r| allocator.free(r);

    try std.testing.expect(result != null);
}
