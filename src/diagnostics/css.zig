//! Unused CSS selector diagnostics for Svelte components
//!
//! Detects CSS selectors that are declared but never used in markup:
//! - Parses CSS from style blocks to extract class/id names
//! - Collects used classes/IDs from class attributes and class: directives
//! - Compares declared vs used, emitting warnings for unused local selectors
//! - Skips :global() selectors (intentionally used outside component)
//! - Handles dynamic classes conservatively (marks as maybe-used)

const std = @import("std");
const Ast = @import("../svelte_parser.zig").Ast;
const Node = @import("../svelte_parser.zig").Node;
const NodeKind = @import("../svelte_parser.zig").NodeKind;
const ElementData = @import("../svelte_parser.zig").ElementData;
const AttributeData = @import("../svelte_parser.zig").AttributeData;
const Diagnostic = @import("../diagnostic.zig").Diagnostic;
const Severity = @import("../diagnostic.zig").Severity;

const SelectorInfo = struct {
    name: []const u8,
    start: u32,
    end: u32,
    is_global: bool,
};

pub fn runDiagnostics(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Step 1: Extract declared selectors from <style> blocks
    var declared_classes: std.ArrayList(SelectorInfo) = .empty;
    var declared_ids: std.ArrayList(SelectorInfo) = .empty;

    for (ast.styles.items) |style| {
        const css_content = ast.source[style.content_start..style.content_end];
        try parseSelectors(allocator, css_content, style.content_start, &declared_classes, &declared_ids);
    }

    // Step 2: Collect used classes/IDs from markup
    var used_classes: std.StringHashMapUnmanaged(void) = .empty;
    var used_ids: std.StringHashMapUnmanaged(void) = .empty;
    var has_dynamic_classes = false;

    for (ast.nodes.items) |node| {
        if (node.kind != .element and node.kind != .component) continue;

        const elem = ast.elements.items[node.data];
        const attrs = ast.attributes.items[elem.attrs_start..elem.attrs_end];

        for (attrs) |attr| {
            // class attribute: class="foo bar baz"
            if (std.mem.eql(u8, attr.name, "class")) {
                if (attr.value) |value| {
                    if (isDynamicValue(value)) {
                        has_dynamic_classes = true;
                    } else {
                        try collectClassNames(allocator, value, &used_classes);
                    }
                }
            }
            // id attribute: id="my-id"
            else if (std.mem.eql(u8, attr.name, "id")) {
                if (attr.value) |value| {
                    if (!isDynamicValue(value)) {
                        try used_ids.put(allocator, value, {});
                    }
                }
            }
            // class: directive (Svelte syntax): class:active={isActive}
            else if (std.mem.startsWith(u8, attr.name, "class:")) {
                const class_name = attr.name[6..];
                if (class_name.len > 0) {
                    try used_classes.put(allocator, class_name, {});
                }
            }
        }
    }

    // Step 3: Report unused selectors (skip global and handle dynamic conservatively)
    for (declared_classes.items) |selector| {
        if (selector.is_global) continue;
        if (has_dynamic_classes) continue; // Conservative: if any dynamic class exists, skip all warnings
        if (!used_classes.contains(selector.name)) {
            try addUnusedDiagnostic(allocator, ast, selector, "class", diagnostics);
        }
    }

    for (declared_ids.items) |selector| {
        if (selector.is_global) continue;
        if (!used_ids.contains(selector.name)) {
            try addUnusedDiagnostic(allocator, ast, selector, "id", diagnostics);
        }
    }
}

fn parseSelectors(
    allocator: std.mem.Allocator,
    css: []const u8,
    base_offset: u32,
    classes: *std.ArrayList(SelectorInfo),
    ids: *std.ArrayList(SelectorInfo),
) !void {
    var i: u32 = 0;
    var in_global = false;
    var global_depth: u32 = 0;

    while (i < css.len) {
        const c = css[i];

        // Skip comments
        if (i + 1 < css.len and css[i] == '/' and css[i + 1] == '*') {
            i += 2;
            while (i + 1 < css.len) {
                if (css[i] == '*' and css[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Track :global()
        if (i + 7 <= css.len and std.mem.eql(u8, css[i..][0..7], ":global")) {
            in_global = true;
            i += 7;
            // Skip whitespace
            while (i < css.len and (css[i] == ' ' or css[i] == '\t' or css[i] == '\n')) {
                i += 1;
            }
            if (i < css.len and css[i] == '(') {
                global_depth = 1;
                i += 1;
            }
            continue;
        }

        // Track parentheses for :global()
        if (in_global and global_depth > 0) {
            if (c == '(') {
                global_depth += 1;
            } else if (c == ')') {
                global_depth -= 1;
                if (global_depth == 0) {
                    in_global = false;
                }
            }
        }

        // Skip rule body
        if (c == '{') {
            var depth: u32 = 1;
            i += 1;
            while (i < css.len and depth > 0) {
                if (css[i] == '{') depth += 1;
                if (css[i] == '}') depth -= 1;
                i += 1;
            }
            in_global = false;
            global_depth = 0;
            continue;
        }

        // Class selector: .classname
        if (c == '.') {
            const start = i;
            i += 1;
            const name_start = i;
            while (i < css.len and isIdentChar(css[i])) {
                i += 1;
            }
            if (i > name_start) {
                const name = css[name_start..i];
                try classes.append(allocator, .{
                    .name = name,
                    .start = base_offset + start,
                    .end = base_offset + i,
                    .is_global = in_global,
                });
            }
            continue;
        }

        // ID selector: #idname
        if (c == '#') {
            const start = i;
            i += 1;
            const name_start = i;
            while (i < css.len and isIdentChar(css[i])) {
                i += 1;
            }
            if (i > name_start) {
                const name = css[name_start..i];
                try ids.append(allocator, .{
                    .name = name,
                    .start = base_offset + start,
                    .end = base_offset + i,
                    .is_global = in_global,
                });
            }
            continue;
        }

        i += 1;
    }
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '-';
}

fn isDynamicValue(value: []const u8) bool {
    // Dynamic values contain expressions: {expr} or template literals
    return std.mem.indexOf(u8, value, "{") != null or
        std.mem.indexOf(u8, value, "`") != null;
}

fn collectClassNames(
    allocator: std.mem.Allocator,
    class_attr: []const u8,
    used: *std.StringHashMapUnmanaged(void),
) !void {
    var iter = std.mem.splitAny(u8, class_attr, " \t\n\r");
    while (iter.next()) |class_name| {
        const trimmed = std.mem.trim(u8, class_name, " \t\n\r");
        if (trimmed.len > 0) {
            try used.put(allocator, trimmed, {});
        }
    }
}

fn addUnusedDiagnostic(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    selector: SelectorInfo,
    selector_type: []const u8,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    const loc = computeLineCol(ast.source, selector.start);

    const message = try std.fmt.allocPrint(
        allocator,
        "Unused CSS selector \"{s}{s}\"",
        .{ if (std.mem.eql(u8, selector_type, "class")) "." else "#", selector.name },
    );

    try diagnostics.append(allocator, .{
        .source = .css,
        .severity = .warning,
        .code = "css-unused-selector",
        .message = message,
        .file_path = ast.file_path,
        .start_line = loc.line,
        .start_col = loc.col,
        .end_line = loc.line,
        .end_col = loc.col + @as(u32, @intCast(selector.name.len)) + 1,
    });
}

fn computeLineCol(source: []const u8, pos: u32) struct { line: u32, col: u32 } {
    var line: u32 = 1;
    var col: u32 = 1;
    var i: u32 = 0;
    while (i < pos and i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .col = col };
}

// ============================================================================
// Tests
// ============================================================================

test "css: unused class selector" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div class="used">content</div>
        \\<style>
        \\.used { color: blue; }
        \\.unused { color: red; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 1);
    try std.testing.expectEqualStrings("css-unused-selector", diagnostics.items[0].code.?);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.items[0].message, ".unused") != null);
}

test "css: all selectors used" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div class="foo bar">content</div>
        \\<style>
        \\.foo { color: blue; }
        \\.bar { color: red; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 0);
}

test "css: class directive usage" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div class:active={isActive}>content</div>
        \\<style>
        \\.active { font-weight: bold; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 0);
}

test "css: global selectors skipped" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div>content</div>
        \\<style>
        \\:global(.external) { color: blue; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 0);
}

test "css: dynamic class suppresses warnings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div class={dynamicClass}>content</div>
        \\<style>
        \\.maybe-used { color: blue; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should not warn because dynamic class might evaluate to .maybe-used
    try std.testing.expect(diagnostics.items.len == 0);
}

test "css: unused id selector" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div id="used-id">content</div>
        \\<style>
        \\#used-id { color: blue; }
        \\#unused-id { color: red; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 1);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.items[0].message, "#unused-id") != null);
}

test "css: element selectors ignored" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div>content</div>
        \\<style>
        \\div { color: blue; }
        \\p { color: red; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Element selectors are not tracked (too complex to analyze statically)
    try std.testing.expect(diagnostics.items.len == 0);
}

test "css: comment handling" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<div class="used">content</div>
        \\<style>
        \\/* .commented-out { color: red; } */
        \\.used { color: blue; }
        \\</style>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should not warn about commented-out selector
    try std.testing.expect(diagnostics.items.len == 0);
}
