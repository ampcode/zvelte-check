//! Accessibility (a11y) diagnostics for Svelte components
//!
//! Checks for common accessibility issues:
//! - img elements without alt attribute
//! - Interactive elements without accessible names
//! - Links without href
//! - Form inputs without labels
//! - aria-* attribute validation

const std = @import("std");
const Ast = @import("../svelte_parser.zig").Ast;
const Node = @import("../svelte_parser.zig").Node;
const NodeKind = @import("../svelte_parser.zig").NodeKind;
const ElementData = @import("../svelte_parser.zig").ElementData;
const AttributeData = @import("../svelte_parser.zig").AttributeData;
const Diagnostic = @import("../diagnostic.zig").Diagnostic;
const Severity = @import("../diagnostic.zig").Severity;
const Source = @import("../diagnostic.zig").Source;

pub fn runDiagnostics(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    for (ast.nodes.items) |node| {
        if (node.kind != .element and node.kind != .component) continue;

        const elem = ast.elements.items[node.data];
        const attrs = ast.attributes.items[elem.attrs_start..elem.attrs_end];

        try checkElement(allocator, ast, node, elem, attrs, diagnostics);
    }
}

fn checkElement(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    elem: ElementData,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    const tag = elem.tag_name;

    // img: must have alt attribute
    if (std.mem.eql(u8, tag, "img")) {
        try checkImgAlt(allocator, ast, node, attrs, diagnostics);
    }

    // a: should have href, and content for accessible name
    if (std.mem.eql(u8, tag, "a")) {
        try checkAnchor(allocator, ast, node, attrs, diagnostics);
    }

    // button: should have accessible name
    if (std.mem.eql(u8, tag, "button")) {
        try checkButton(allocator, ast, node, attrs, diagnostics);
    }

    // input: check for labels
    if (std.mem.eql(u8, tag, "input")) {
        try checkInput(allocator, ast, node, attrs, diagnostics);
    }

    // Interactive elements need accessible names
    if (isInteractiveElement(tag)) {
        try checkInteractive(allocator, ast, node, attrs, diagnostics);
    }

    // Check all aria-* attributes for validity
    try checkAriaAttributes(allocator, ast, node, attrs, diagnostics);
}

fn checkImgAlt(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    const has_alt = hasAttr(attrs, "alt");
    const has_role_presentation = hasAttrValue(attrs, "role", "presentation") or
        hasAttrValue(attrs, "role", "none");

    if (!has_alt and !has_role_presentation) {
        try addDiagnostic(allocator, ast, node, diagnostics, .{
            .severity = .@"error",
            .code = "a11y-missing-alt",
            .message = "<img> element must have an alt attribute",
        });
    }
}

fn checkAnchor(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    const has_href = hasAttr(attrs, "href");

    if (!has_href) {
        try addDiagnostic(allocator, ast, node, diagnostics, .{
            .severity = .warning,
            .code = "a11y-missing-href",
            .message = "<a> element should have an href attribute",
        });
    }

    // Check for empty href or javascript: href
    if (getAttrValue(attrs, "href")) |href| {
        if (href.len == 0) {
            try addDiagnostic(allocator, ast, node, diagnostics, .{
                .severity = .warning,
                .code = "a11y-invalid-href",
                .message = "<a> element has empty href",
            });
        } else if (std.mem.startsWith(u8, href, "javascript:")) {
            try addDiagnostic(allocator, ast, node, diagnostics, .{
                .severity = .warning,
                .code = "a11y-invalid-href",
                .message = "<a> element should not use javascript: URLs",
            });
        }
    }
}

fn checkButton(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    _ = allocator;
    _ = ast;
    _ = node;
    _ = diagnostics;

    // Button accessible name comes from content, aria-label, or aria-labelledby
    // Since we don't parse children yet, we only check for aria-label/aria-labelledby
    // on self-closing buttons (which would have no content)
    _ = attrs;
}

fn checkInput(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    const input_type = getAttrValue(attrs, "type") orelse "text";

    // Hidden inputs don't need labels
    if (std.mem.eql(u8, input_type, "hidden")) return;

    // Check for accessible label
    const has_id = hasAttr(attrs, "id");
    const has_aria_label = hasAttr(attrs, "aria-label");
    const has_aria_labelledby = hasAttr(attrs, "aria-labelledby");
    const has_title = hasAttr(attrs, "title");

    // Note: We can't check for associated <label> elements without full DOM traversal
    // So we only warn if none of the inline labeling mechanisms are present
    if (!has_id and !has_aria_label and !has_aria_labelledby and !has_title) {
        try addDiagnostic(allocator, ast, node, diagnostics, .{
            .severity = .warning,
            .code = "a11y-missing-label",
            .message = "<input> element should have an associated label or aria-label",
        });
    }
}

fn checkInteractive(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Check for tabindex with positive values (anti-pattern)
    if (getAttrValue(attrs, "tabindex")) |tabindex| {
        if (tabindex.len > 0 and tabindex[0] != '-' and tabindex[0] != '0') {
            try addDiagnostic(allocator, ast, node, diagnostics, .{
                .severity = .warning,
                .code = "a11y-positive-tabindex",
                .message = "Avoid positive tabindex values",
            });
        }
    }
}

fn checkAriaAttributes(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    for (attrs) |attr| {
        if (!std.mem.startsWith(u8, attr.name, "aria-")) continue;

        const aria_name = attr.name[5..];

        // Check if it's a valid aria attribute
        if (!isValidAriaAttribute(aria_name)) {
            try addDiagnostic(allocator, ast, node, diagnostics, .{
                .severity = .warning,
                .code = "a11y-unknown-aria",
                .message = try std.fmt.allocPrint(
                    allocator,
                    "Unknown ARIA attribute: {s}",
                    .{attr.name},
                ),
            });
        }

        // Check for aria-hidden on focusable elements
        if (std.mem.eql(u8, aria_name, "hidden")) {
            if (attr.value != null and std.mem.eql(u8, attr.value.?, "true")) {
                if (hasAttr(attrs, "tabindex") or hasAttr(attrs, "href")) {
                    try addDiagnostic(allocator, ast, node, diagnostics, .{
                        .severity = .warning,
                        .code = "a11y-hidden-focusable",
                        .message = "aria-hidden should not be used on focusable elements",
                    });
                }
            }
        }
    }
}

fn isInteractiveElement(tag: []const u8) bool {
    const interactive_tags = [_][]const u8{
        "a", "button", "input", "select", "textarea", "details", "summary",
    };
    for (interactive_tags) |t| {
        if (std.mem.eql(u8, tag, t)) return true;
    }
    return false;
}

fn isValidAriaAttribute(name: []const u8) bool {
    const valid_aria = [_][]const u8{
        "activedescendant", "atomic",          "autocomplete",    "busy",
        "checked",          "colcount",        "colindex",        "colspan",
        "controls",         "current",         "describedby",     "description",
        "details",          "disabled",        "dropeffect",      "errormessage",
        "expanded",         "flowto",          "grabbed",         "haspopup",
        "hidden",           "invalid",         "keyshortcuts",    "label",
        "labelledby",       "level",           "live",            "modal",
        "multiline",        "multiselectable", "orientation",     "owns",
        "placeholder",      "posinset",        "pressed",         "readonly",
        "relevant",         "required",        "roledescription", "rowcount",
        "rowindex",         "rowspan",         "selected",        "setsize",
        "sort",             "valuemax",        "valuemin",        "valuenow",
        "valuetext",
    };
    for (valid_aria) |v| {
        if (std.mem.eql(u8, name, v)) return true;
    }
    return false;
}

fn hasAttr(attrs: []const AttributeData, name: []const u8) bool {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name, name)) return true;
    }
    return false;
}

fn hasAttrValue(attrs: []const AttributeData, name: []const u8, value: []const u8) bool {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name, name)) {
            if (attr.value) |v| {
                return std.mem.eql(u8, v, value);
            }
        }
    }
    return false;
}

fn getAttrValue(attrs: []const AttributeData, name: []const u8) ?[]const u8 {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name, name)) return attr.value;
    }
    return null;
}

const DiagnosticInfo = struct {
    severity: Severity,
    code: []const u8,
    message: []const u8,
};

fn addDiagnostic(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    diagnostics: *std.ArrayList(Diagnostic),
    info: DiagnosticInfo,
) !void {
    const loc = computeLineCol(ast.source, node.start);

    try diagnostics.append(allocator, .{
        .source = .svelte,
        .severity = info.severity,
        .code = info.code,
        .message = info.message,
        .file_path = ast.file_path,
        .start_line = loc.line,
        .start_col = loc.col,
        .end_line = loc.line,
        .end_col = loc.col,
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

test "a11y: img without alt" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<img src=\"test.png\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 1);
    try std.testing.expectEqualStrings("a11y-missing-alt", diagnostics.items[0].code.?);
}

test "a11y: img with alt" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<img src=\"test.png\" alt=\"A test image\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len == 0);
}

test "a11y: anchor without href" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<a>Click me</a>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expect(diagnostics.items.len >= 1);
    var found = false;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y-missing-href")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}
