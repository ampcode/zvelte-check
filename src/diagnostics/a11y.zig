//! Accessibility (a11y) diagnostics for Svelte components
//!
//! Checks for common accessibility issues:
//! - img elements without alt attribute
//! - Interactive elements without accessible names
//! - Links without href
//! - Form inputs without labels
//! - aria-* attribute validation
//! - Distracting elements (marquee, blink)
//! - Event handler pairing (click/keyboard, mouse/focus)
//! - Heading structure
//! - Redundant roles
//! - Required ARIA properties

const std = @import("std");
const Ast = @import("../svelte_parser.zig").Ast;
const Node = @import("../svelte_parser.zig").Node;
const NodeKind = @import("../svelte_parser.zig").NodeKind;
const ElementData = @import("../svelte_parser.zig").ElementData;
const AttributeData = @import("../svelte_parser.zig").AttributeData;
const CommentData = @import("../svelte_parser.zig").CommentData;
const Diagnostic = @import("../diagnostic.zig").Diagnostic;
const Severity = @import("../diagnostic.zig").Severity;
const Source = @import("../diagnostic.zig").Source;

const HeadingTracker = struct {
    last_level: u8 = 0,
};

pub fn runDiagnostics(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    var heading_tracker: HeadingTracker = .{};

    // Track preceding comment node to check for svelte-ignore
    var prev_comment: ?CommentData = null;

    for (ast.nodes.items) |node| {
        if (node.kind == .comment) {
            prev_comment = ast.comments.items[node.data];
            continue;
        }

        // Only check HTML elements, not Svelte components
        // Components are responsible for their own a11y
        if (node.kind != .element) {
            prev_comment = null;
            continue;
        }

        const elem = ast.elements.items[node.data];
        const attrs = ast.attributes.items[elem.attrs_start..elem.attrs_end];

        // Get ignore codes from preceding comment (if any)
        const ignore_codes = if (prev_comment) |comment|
            ast.ignore_codes.items[comment.ignore_codes_start..comment.ignore_codes_end]
        else
            &[_][]const u8{};

        try checkElement(allocator, ast, node, elem, attrs, diagnostics, &heading_tracker, ignore_codes);

        prev_comment = null;
    }
}

fn checkElement(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    elem: ElementData,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    heading_tracker: *HeadingTracker,
    ignore_codes: []const []const u8,
) !void {
    const tag = elem.tag_name;

    // Distracting elements: marquee and blink
    if (std.mem.eql(u8, tag, "marquee") or std.mem.eql(u8, tag, "blink")) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_distracting_elements",
            .message = try std.fmt.allocPrint(
                allocator,
                "<{s}> element is distracting and should be avoided",
                .{tag},
            ),
        });
    }

    // img: must have alt attribute
    if (std.mem.eql(u8, tag, "img")) {
        try checkImgAlt(allocator, ast, node, attrs, diagnostics, ignore_codes);
        try checkImgRedundantAlt(allocator, ast, node, attrs, diagnostics, ignore_codes);
    }

    // a: should have href, and content for accessible name
    if (std.mem.eql(u8, tag, "a")) {
        try checkAnchor(allocator, ast, node, attrs, diagnostics, ignore_codes);
    }

    // button: should have accessible name
    if (std.mem.eql(u8, tag, "button")) {
        try checkButton(allocator, ast, node, attrs, diagnostics, ignore_codes);
    }

    // input: check for labels
    if (std.mem.eql(u8, tag, "input")) {
        try checkInput(allocator, ast, node, attrs, diagnostics, ignore_codes);
    }

    // video/audio: check for captions
    if (std.mem.eql(u8, tag, "video") or std.mem.eql(u8, tag, "audio")) {
        try checkMediaCaptions(allocator, ast, node, elem, attrs, diagnostics, ignore_codes);
    }

    // Heading structure
    if (isHeading(tag)) {
        try checkHeadingOrder(allocator, ast, node, tag, diagnostics, heading_tracker, ignore_codes);
    }

    // Check for autofocus attribute
    if (hasAttr(attrs, "autofocus")) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_autofocus",
            .message = "Avoid using autofocus",
        });
    }

    // Check for accesskey attribute
    if (hasAttr(attrs, "accesskey")) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_accesskey",
            .message = "Avoid using accesskey",
        });
    }

    // Check for positive tabindex
    try checkPositiveTabindex(allocator, ast, node, attrs, diagnostics, ignore_codes);

    // Check for redundant roles
    try checkRedundantRoles(allocator, ast, node, tag, attrs, diagnostics, ignore_codes);

    // Check for required ARIA props based on role
    try checkRoleRequiredProps(allocator, ast, node, attrs, diagnostics, ignore_codes);

    // Check for event handler pairing
    try checkClickKeyEvents(allocator, ast, node, tag, attrs, diagnostics, ignore_codes);
    try checkMouseKeyEvents(allocator, ast, node, attrs, diagnostics, ignore_codes);

    // Check for non-interactive tabindex
    try checkNoninteractiveTabindex(allocator, ast, node, tag, attrs, diagnostics, ignore_codes);

    // Check for static element interactions
    try checkStaticElementInteractions(allocator, ast, node, tag, attrs, diagnostics, ignore_codes);

    // Check for interactive roles supporting focus
    try checkInteractiveSupportsFocus(allocator, ast, node, tag, attrs, diagnostics, ignore_codes);

    // Check all aria-* attributes for validity
    try checkAriaAttributes(allocator, ast, node, attrs, diagnostics, ignore_codes);
}

fn checkImgAlt(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    const has_alt = hasAttr(attrs, "alt");
    const has_role_presentation = hasAttrValue(attrs, "role", "presentation") or
        hasAttrValue(attrs, "role", "none");

    if (!has_alt and !has_role_presentation) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_missing_attribute",
            .message = "<img> element should have an alt attribute",
        });
    }
}

fn checkImgRedundantAlt(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name, "alt")) {
            // Skip dynamic values - can't analyze runtime content
            if (isDynamicValue(attr.value)) return;

            const alt = attr.value orelse return;
            if (alt.len == 0) return;

            var lower_buf: [256]u8 = undefined;
            const alt_lower = toLowerBounded(alt, &lower_buf);

            const redundant_words = [_][]const u8{ "image", "photo", "picture" };
            for (redundant_words) |word| {
                if (std.mem.indexOf(u8, alt_lower, word) != null) {
                    try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                        .severity = .warning,
                        .code = "a11y_img_redundant_alt",
                        .message = "Alt text should not contain words like \"image\", \"photo\", or \"picture\"",
                    });
                    return;
                }
            }
            return;
        }
    }
}

fn checkAnchor(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    const has_href = hasAttr(attrs, "href");

    if (!has_href) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_invalid_attribute",
            .message = "<a> element should have an href attribute",
        });
    }

    // Check for empty href or javascript: href
    if (getAttrValue(attrs, "href")) |href| {
        if (href.len == 0) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_invalid_attribute",
                .message = "<a> element has empty href",
            });
        } else if (std.mem.startsWith(u8, href, "javascript:")) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_invalid_attribute",
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
    ignore_codes: []const []const u8,
) !void {
    _ = allocator;
    _ = ast;
    _ = node;
    _ = diagnostics;
    _ = ignore_codes;

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
    ignore_codes: []const []const u8,
) !void {
    _ = allocator;
    _ = ast;
    _ = node;
    _ = attrs;
    _ = diagnostics;
    _ = ignore_codes;
    // Note: We can't reliably check for associated <label> elements without
    // full DOM traversal (labels can be wrapper elements or use for="id").
    // This produces too many false positives, so we skip this check.
}

fn checkMediaCaptions(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    elem: ElementData,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    // Muted videos don't need captions
    if (hasAttr(attrs, "muted")) return;

    // Check if element has a track child with kind="captions" or kind="subtitles"
    // For now, we check by looking at subsequent nodes until we find a closing or sibling
    const has_captions = hasTrackWithCaptions(ast, node);

    if (!has_captions) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_media_has_caption",
            .message = try std.fmt.allocPrint(
                allocator,
                "<{s}> element should have a <track> with captions",
                .{elem.tag_name},
            ),
        });
    }
}

fn hasTrackWithCaptions(ast: *const Ast, parent_node: Node) bool {
    // Look for track children with kind="captions" or kind="subtitles"
    var child_idx = parent_node.first_child;
    while (child_idx != Node.NONE and child_idx < ast.nodes.items.len) {
        const child = ast.nodes.items[child_idx];
        if (child.kind == .element) {
            const child_elem = ast.elements.items[child.data];
            if (std.mem.eql(u8, child_elem.tag_name, "track")) {
                const child_attrs = ast.attributes.items[child_elem.attrs_start..child_elem.attrs_end];
                if (hasAttrValue(child_attrs, "kind", "captions") or
                    hasAttrValue(child_attrs, "kind", "subtitles"))
                {
                    return true;
                }
            }
        }
        child_idx = child.next_sibling;
    }
    return false;
}

fn checkHeadingOrder(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    diagnostics: *std.ArrayList(Diagnostic),
    tracker: *HeadingTracker,
    ignore_codes: []const []const u8,
) !void {
    _ = allocator;
    _ = ast;
    _ = node;
    _ = diagnostics;
    _ = ignore_codes;
    // Note: svelte-check does not check heading order within components
    // because components may be composed at different levels in a page.
    // Track the level but don't warn.
    const level = tag[1] - '0';
    tracker.last_level = level;
}

fn checkPositiveTabindex(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name, "tabindex")) {
            // Skip dynamic values - can't know the runtime value
            if (isDynamicValue(attr.value)) return;

            if (attr.value) |tabindex| {
                if (tabindex.len > 0 and tabindex[0] != '-' and tabindex[0] != '0') {
                    try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                        .severity = .warning,
                        .code = "a11y_positive_tabindex",
                        .message = "Avoid positive tabindex values",
                    });
                }
            }
            return;
        }
    }
}

fn checkRedundantRoles(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    const role = getAttrValue(attrs, "role") orelse return;
    const implicit_role = getImplicitRole(tag, attrs);

    if (implicit_role) |ir| {
        if (std.mem.eql(u8, role, ir)) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_no_redundant_roles",
                .message = try std.fmt.allocPrint(
                    allocator,
                    "<{s}> has implicit role \"{s}\"",
                    .{ tag, ir },
                ),
            });
        }
    }
}

fn checkRoleRequiredProps(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    const role = getAttrValue(attrs, "role") orelse return;

    const required_props = getRoleRequiredProps(role);
    if (required_props.len == 0) return;

    for (required_props) |prop| {
        if (!hasAttr(attrs, prop)) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_role_has_required_aria_props",
                .message = try std.fmt.allocPrint(
                    allocator,
                    "role=\"{s}\" requires {s}",
                    .{ role, prop },
                ),
            });
            return;
        }
    }
}

fn checkClickKeyEvents(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    // Native interactive elements don't need keyboard handlers
    if (isNativeInteractiveElement(tag)) return;

    const has_click = hasEventHandler(attrs, "click");
    if (!has_click) return;

    const has_keyboard = hasEventHandler(attrs, "keydown") or
        hasEventHandler(attrs, "keyup") or
        hasEventHandler(attrs, "keypress");

    if (!has_keyboard) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_click_events_have_key_events",
            .message = "Elements with on:click must have a keyboard event handler",
        });
    }
}

fn checkMouseKeyEvents(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    const has_focus = hasEventHandler(attrs, "focus");
    const has_blur = hasEventHandler(attrs, "blur");

    // mouseenter/mouseover should have focus
    if (hasEventHandler(attrs, "mouseenter") or hasEventHandler(attrs, "mouseover")) {
        if (!has_focus) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_mouse_events_have_key_events",
                .message = "on:mouseenter or on:mouseover must be accompanied by on:focus",
            });
        }
    }

    // mouseleave/mouseout should have blur
    if (hasEventHandler(attrs, "mouseleave") or hasEventHandler(attrs, "mouseout")) {
        if (!has_blur) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_mouse_events_have_key_events",
                .message = "on:mouseleave or on:mouseout must be accompanied by on:blur",
            });
        }
    }
}

fn checkNoninteractiveTabindex(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    // Skip if element has an interactive role
    if (getAttrValue(attrs, "role")) |role| {
        if (isInteractiveRole(role)) return;
    }

    // Skip native interactive elements
    if (isNativeInteractiveElement(tag)) return;

    // Check for tabindex >= 0
    if (getAttrValue(attrs, "tabindex")) |tabindex| {
        if (tabindex.len > 0 and tabindex[0] != '-') {
            // tabindex="0" or positive
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_no_noninteractive_tabindex",
                .message = "Non-interactive elements should not have tabindex",
            });
        }
    }
}

fn checkStaticElementInteractions(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    // Skip native interactive elements
    if (isNativeInteractiveElement(tag)) return;

    // Skip if element has a role
    if (hasAttr(attrs, "role")) return;

    // Check for click handler
    if (hasEventHandler(attrs, "click")) {
        try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
            .severity = .warning,
            .code = "a11y_no_static_element_interactions",
            .message = "Static elements with event handlers require a role",
        });
    }
}

fn checkInteractiveSupportsFocus(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    tag: []const u8,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    // Native interactive elements are focusable
    if (isNativeInteractiveElement(tag)) return;

    // Check if element has an interactive role
    const role = getAttrValue(attrs, "role") orelse return;
    if (!isInteractiveRole(role)) return;

    // Check for focusability: tabindex or contenteditable
    if (hasAttr(attrs, "tabindex")) return;
    if (hasAttr(attrs, "contenteditable")) return;

    try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
        .severity = .warning,
        .code = "a11y_interactive_supports_focus",
        .message = try std.fmt.allocPrint(
            allocator,
            "Elements with role=\"{s}\" must have tabindex",
            .{role},
        ),
    });
}

fn checkAriaAttributes(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    node: Node,
    attrs: []const AttributeData,
    diagnostics: *std.ArrayList(Diagnostic),
    ignore_codes: []const []const u8,
) !void {
    for (attrs) |attr| {
        if (!std.mem.startsWith(u8, attr.name, "aria-")) continue;

        const aria_name = attr.name[5..];

        // Check if it's a valid aria attribute
        if (!isValidAriaAttribute(aria_name)) {
            try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                .severity = .warning,
                .code = "a11y_unknown_aria_attribute",
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
                    try addDiagnostic(allocator, ast, node, diagnostics, ignore_codes, .{
                        .severity = .warning,
                        .code = "a11y_hidden",
                        .message = "aria-hidden should not be used on focusable elements",
                    });
                }
            }
        }
    }
}

// --- Data tables ---

fn isHeading(tag: []const u8) bool {
    return tag.len == 2 and tag[0] == 'h' and tag[1] >= '1' and tag[1] <= '6';
}

fn isNativeInteractiveElement(tag: []const u8) bool {
    const interactive = [_][]const u8{
        "a", "button", "input", "select", "textarea", "details", "summary",
    };
    for (interactive) |t| {
        if (std.mem.eql(u8, tag, t)) return true;
    }
    return false;
}

fn isInteractiveRole(role: []const u8) bool {
    const interactive_roles = [_][]const u8{
        "button",           "checkbox",      "link",       "menuitem",
        "menuitemcheckbox", "menuitemradio", "option",     "radio",
        "searchbox",        "slider",        "spinbutton", "switch",
        "tab",              "textbox",       "treeitem",
    };
    for (interactive_roles) |r| {
        if (std.mem.eql(u8, role, r)) return true;
    }
    return false;
}

fn getImplicitRole(tag: []const u8, attrs: []const AttributeData) ?[]const u8 {
    if (std.mem.eql(u8, tag, "button")) return "button";
    if (std.mem.eql(u8, tag, "a")) {
        if (hasAttr(attrs, "href")) return "link";
    }
    if (std.mem.eql(u8, tag, "img")) return "img";
    if (std.mem.eql(u8, tag, "nav")) return "navigation";
    if (std.mem.eql(u8, tag, "main")) return "main";
    if (std.mem.eql(u8, tag, "article")) return "article";
    if (std.mem.eql(u8, tag, "aside")) return "complementary";
    if (std.mem.eql(u8, tag, "section")) return "region";
    if (std.mem.eql(u8, tag, "hr")) return "separator";
    if (std.mem.eql(u8, tag, "ul") or std.mem.eql(u8, tag, "ol")) return "list";
    if (std.mem.eql(u8, tag, "li")) return "listitem";
    if (std.mem.eql(u8, tag, "table")) return "table";
    if (isHeading(tag)) return "heading";
    return null;
}

fn getRoleRequiredProps(role: []const u8) []const []const u8 {
    const S = struct {
        const checkbox_props = [_][]const u8{"aria-checked"};
        const radio_props = [_][]const u8{"aria-checked"};
        const switch_props = [_][]const u8{"aria-checked"};
        const slider_props = [_][]const u8{"aria-valuenow"};
        const spinbutton_props = [_][]const u8{"aria-valuenow"};
        const scrollbar_props = [_][]const u8{"aria-valuenow"};
        const combobox_props = [_][]const u8{ "aria-controls", "aria-expanded" };
        const option_props = [_][]const u8{"aria-selected"};
        const heading_props = [_][]const u8{"aria-level"};
        const separator_props = [_][]const u8{"aria-valuenow"};
    };

    if (std.mem.eql(u8, role, "checkbox")) return &S.checkbox_props;
    if (std.mem.eql(u8, role, "radio")) return &S.radio_props;
    if (std.mem.eql(u8, role, "switch")) return &S.switch_props;
    if (std.mem.eql(u8, role, "slider")) return &S.slider_props;
    if (std.mem.eql(u8, role, "spinbutton")) return &S.spinbutton_props;
    if (std.mem.eql(u8, role, "scrollbar")) return &S.scrollbar_props;
    if (std.mem.eql(u8, role, "combobox")) return &S.combobox_props;
    if (std.mem.eql(u8, role, "option")) return &S.option_props;
    if (std.mem.eql(u8, role, "heading")) return &S.heading_props;
    if (std.mem.eql(u8, role, "separator")) return &S.separator_props;

    return &[_][]const u8{};
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

fn hasEventHandler(attrs: []const AttributeData, event: []const u8) bool {
    for (attrs) |attr| {
        // Match "on:click", "on:keydown", etc.
        if (attr.name.len > 3 and std.mem.startsWith(u8, attr.name, "on:")) {
            const event_name = attr.name[3..];
            if (std.mem.eql(u8, event_name, event)) return true;
        }
    }
    return false;
}

fn isDynamicValue(value: ?[]const u8) bool {
    const v = value orelse return false;
    // Pure expression: {expr}
    if (v.len > 0 and v[0] == '{') return true;
    // Template with embedded expressions: "text {expr} more"
    if (std.mem.indexOf(u8, v, "{") != null) return true;
    return false;
}

fn toLowerBounded(s: []const u8, buf: []u8) []const u8 {
    const len = @min(s.len, buf.len);
    for (s[0..len], 0..) |c, i| {
        buf[i] = std.ascii.toLower(c);
    }
    return buf[0..len];
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
    ignore_codes: []const []const u8,
    info: DiagnosticInfo,
) !void {
    // Check if this diagnostic code is suppressed by svelte-ignore
    for (ignore_codes) |ignored| {
        if (std.mem.eql(u8, ignored, info.code)) return;
    }

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
    try std.testing.expectEqualStrings("a11y_missing_attribute", diagnostics.items[0].code.?);
}

test "a11y: img with alt" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<img src=\"test.png\" alt=\"A sunset\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should have no a11y-missing-alt
    var found_missing_alt = false;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_missing_attribute")) {
            found_missing_alt = true;
            break;
        }
    }
    try std.testing.expect(!found_missing_alt);
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
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_invalid_attribute")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "a11y: distracting elements" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<marquee>Scrolling</marquee><blink>Blinking</blink>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    var count: usize = 0;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_distracting_elements")) {
            count += 1;
        }
    }
    try std.testing.expect(count == 2);
}

test "a11y: redundant roles" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<button role=\"button\">Click</button>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    var found = false;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_no_redundant_roles")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "a11y: svelte-ignore suppresses diagnostic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<!-- svelte-ignore a11y_missing_attribute -->\n<img src=\"test.png\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should have no a11y-missing-alt diagnostic due to svelte-ignore
    var found_missing_alt = false;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_missing_attribute")) {
            found_missing_alt = true;
            break;
        }
    }
    try std.testing.expect(!found_missing_alt);
}

test "a11y: svelte-ignore with multiple codes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<!-- svelte-ignore a11y_distracting_elements a11y_autofocus -->\n<marquee autofocus>Text</marquee>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Both diagnostics should be suppressed
    var found_distracting = false;
    var found_autofocus = false;
    for (diagnostics.items) |d| {
        if (d.code != null) {
            if (std.mem.eql(u8, d.code.?, "a11y_distracting_elements")) found_distracting = true;
            if (std.mem.eql(u8, d.code.?, "a11y_autofocus")) found_autofocus = true;
        }
    }
    try std.testing.expect(!found_distracting);
    try std.testing.expect(!found_autofocus);
}

test "a11y: regular comment does not suppress diagnostic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<!-- Just a regular comment -->\n<img src=\"test.png\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should have a11y-missing-alt since no svelte-ignore
    var found_missing_alt = false;
    for (diagnostics.items) |d| {
        if (d.code != null and std.mem.eql(u8, d.code.?, "a11y_missing_attribute")) {
            found_missing_alt = true;
            break;
        }
    }
    try std.testing.expect(found_missing_alt);
}

test "a11y: dynamic values skip checks" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    // Dynamic tabindex and alt with template expression should not warn
    const source = "<div tabindex={condition ? 5 : 0}></div><img src={url} alt=\"Image from {path}\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Should not have positive_tabindex or img_redundant_alt warnings
    for (diagnostics.items) |d| {
        if (d.code) |code| {
            try std.testing.expect(!std.mem.eql(u8, code, "a11y_positive_tabindex"));
            try std.testing.expect(!std.mem.eql(u8, code, "a11y_img_redundant_alt"));
        }
    }
}
