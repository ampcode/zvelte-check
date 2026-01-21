//! Svelte parser - builds AST from tokens
//!
//! Produces a flat, arena-allocated AST with:
//! - Script blocks (with lang attribute)
//! - Style blocks
//! - Markup (elements, text, Svelte blocks)

const std = @import("std");
const Lexer = @import("svelte_lexer.zig").Lexer;
const Token = @import("svelte_lexer.zig").Token;
const TokenKind = @import("svelte_lexer.zig").TokenKind;
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const a11y = @import("diagnostics/a11y.zig");
const css = @import("diagnostics/css.zig");
const component = @import("diagnostics/component.zig");

pub const NodeKind = enum(u8) {
    root,
    element,
    text,
    script,
    style,
    expression, // {expr}
    if_block,
    else_block,
    then_block, // {:then value}
    catch_block, // {:catch error}
    each_block,
    await_block,
    key_block,
    snippet,
    render,
    html,
    const_tag,
    debug_tag,
    attribute,
    spread_attribute,
    directive, // on:, bind:, class:, etc.
    slot,
    component, // PascalCase or svelte:component
    comment, // HTML comment, stores index into comments array
};

pub const Node = struct {
    kind: NodeKind,
    start: u32,
    end: u32,
    first_child: u32, // index or sentinel
    next_sibling: u32,
    data: u32, // index into kind-specific arrays

    pub const NONE: u32 = std.math.maxInt(u32);
};

pub const ScriptData = struct {
    content_start: u32,
    content_end: u32,
    lang: ?[]const u8, // "ts" or null
    context: ?[]const u8, // "module" for <script context="module">
    generics: ?[]const u8, // "T" or "T extends SomeType" for <script generics="T">
};

pub const StyleData = struct {
    content_start: u32,
    content_end: u32,
    lang: ?[]const u8, // "scss", "less", etc.
};

pub const AttributeData = struct {
    name: []const u8,
    value: ?[]const u8, // null for valueless attributes like "disabled"
    start: u32,
    end: u32,
};

pub const ElementData = struct {
    tag_name: []const u8,
    is_self_closing: bool,
    is_component: bool, // PascalCase or svelte:*
    attrs_start: u32, // index into attributes array
    attrs_end: u32, // exclusive end index
};

pub const CommentData = struct {
    content: []const u8, // The text inside <!-- ... -->
    ignore_codes_start: u32, // Index into ignore_codes array
    ignore_codes_end: u32, // Exclusive end index
};

/// Info about an open element for unclosed tag tracking
pub const OpenElement = struct {
    tag_name: []const u8,
    start: u32, // Position in source
};

pub const Ast = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    file_path: []const u8,

    nodes: std.ArrayList(Node),
    scripts: std.ArrayList(ScriptData),
    styles: std.ArrayList(StyleData),
    elements: std.ArrayList(ElementData),
    attributes: std.ArrayList(AttributeData),
    comments: std.ArrayList(CommentData),
    ignore_codes: std.ArrayList([]const u8), // Parsed svelte-ignore codes
    parse_errors: std.ArrayList(Diagnostic), // Parser errors (unclosed tags, etc.)

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8) Ast {
        return .{
            .allocator = allocator,
            .source = source,
            .file_path = file_path,
            .nodes = .empty,
            .scripts = .empty,
            .styles = .empty,
            .elements = .empty,
            .attributes = .empty,
            .comments = .empty,
            .ignore_codes = .empty,
            .parse_errors = .empty,
        };
    }

    // No deinit needed - arena handles cleanup

    pub const DiagnosticSources = struct {
        svelte: bool = true,
        css: bool = true,
    };

    pub fn runDiagnostics(
        self: *const Ast,
        allocator: std.mem.Allocator,
        diagnostics: *std.ArrayList(Diagnostic),
        sources: DiagnosticSources,
    ) !void {
        // Include parser errors (unclosed tags, etc.)
        try diagnostics.appendSlice(allocator, self.parse_errors.items);

        if (sources.svelte) {
            try a11y.runDiagnostics(allocator, self, diagnostics);
            try component.runDiagnostics(allocator, self, diagnostics);
        }
        if (sources.css) {
            try css.runDiagnostics(allocator, self, diagnostics);
        }
    }
};

/// HTML void elements that don't need closing tags
const VOID_ELEMENTS = [_][]const u8{
    "area", "base", "br",    "col",    "embed", "hr",  "img", "input",
    "link", "meta", "param", "source", "track", "wbr",
};

fn isVoidElement(tag_name: []const u8) bool {
    for (VOID_ELEMENTS) |void_tag| {
        if (std.ascii.eqlIgnoreCase(tag_name, void_tag)) return true;
    }
    return false;
}

/// Find the matching closing brace for an opening brace at `start`.
/// Properly handles nested braces, strings (single, double, backtick), and comments.
/// Returns the position AFTER the closing brace.
fn findMatchingBrace(source: []const u8, start: u32) u32 {
    var i: usize = start;
    var depth: u32 = 0;

    while (i < source.len) {
        const c = source[i];

        // Skip string literals
        if (c == '"' or c == '\'' or c == '`') {
            i = skipJsString(source, i);
            continue;
        }

        // Skip line comments
        if (c == '/' and i + 1 < source.len and source[i + 1] == '/') {
            i += 2;
            while (i < source.len and source[i] != '\n') : (i += 1) {}
            continue;
        }

        // Skip block comments
        if (c == '/' and i + 1 < source.len and source[i + 1] == '*') {
            i += 2;
            while (i + 1 < source.len) {
                if (source[i] == '*' and source[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Track brace depth
        if (c == '{') {
            depth += 1;
        } else if (c == '}') {
            depth -= 1;
            if (depth == 0) {
                return @intCast(i + 1); // Return position after the closing brace
            }
        }

        i += 1;
    }

    // If no matching brace found, return end of source
    return @intCast(source.len);
}

/// Skip a JavaScript string literal (single, double, or backtick quoted).
/// Handles escape sequences and template literal expressions.
/// Returns the position after the closing quote.
fn skipJsString(source: []const u8, start: usize) usize {
    if (start >= source.len) return start;
    const quote = source[start];
    var i = start + 1;

    while (i < source.len) {
        const c = source[i];

        if (c == quote) {
            return i + 1; // Past the closing quote
        }

        if (c == '\\' and i + 1 < source.len) {
            i += 2; // Skip escape sequence
            continue;
        }

        // For template literals, handle ${...} expressions
        if (quote == '`' and c == '$' and i + 1 < source.len and source[i + 1] == '{') {
            // Find the matching } for this template expression
            var expr_depth: u32 = 0;
            i += 1; // Skip $
            while (i < source.len) {
                const ec = source[i];

                // Skip line comments inside expressions
                if (ec == '/' and i + 1 < source.len and source[i + 1] == '/') {
                    i += 2;
                    while (i < source.len and source[i] != '\n') : (i += 1) {}
                    continue;
                }

                // Skip block comments inside expressions
                if (ec == '/' and i + 1 < source.len and source[i + 1] == '*') {
                    i += 2;
                    while (i + 1 < source.len) {
                        if (source[i] == '*' and source[i + 1] == '/') {
                            i += 2;
                            break;
                        }
                        i += 1;
                    }
                    continue;
                }

                if (ec == '{') expr_depth += 1;
                if (ec == '}') {
                    expr_depth -= 1;
                    if (expr_depth == 0) {
                        i += 1;
                        break;
                    }
                }
                // Skip nested strings inside template expressions
                if (ec == '"' or ec == '\'' or ec == '`') {
                    i = skipJsString(source, i);
                    continue;
                }
                i += 1;
            }
            continue;
        }

        i += 1;
    }

    return i; // Unterminated string - return end of source
}

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    current: Token,
    source: []const u8,
    file_path: []const u8,
    element_stack: std.ArrayList(OpenElement),

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8) Parser {
        var lexer = Lexer.init(source, file_path);
        const first = lexer.next();
        return .{
            .allocator = allocator,
            .lexer = lexer,
            .current = first,
            .source = source,
            .file_path = file_path,
            .element_stack = .empty,
        };
    }

    pub fn parse(self: *Parser) !Ast {
        var ast = Ast.init(self.allocator, self.source, self.file_path);

        // Add root node
        try ast.nodes.append(self.allocator, .{
            .kind = .root,
            .start = 0,
            .end = @intCast(self.source.len),
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        var last_child: u32 = Node.NONE;

        while (self.current.kind != .eof) {
            const child_idx = try self.parseNode(&ast);
            if (child_idx != Node.NONE) {
                if (last_child == Node.NONE) {
                    ast.nodes.items[0].first_child = child_idx;
                } else {
                    ast.nodes.items[last_child].next_sibling = child_idx;
                }
                last_child = child_idx;
            }
        }

        // Report any unclosed elements
        for (self.element_stack.items) |open_elem| {
            const loc = computeLineCol(self.source, open_elem.start);
            try ast.parse_errors.append(self.allocator, .{
                .source = .svelte,
                .severity = .@"error",
                .code = null,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "`<{s}>` was left open",
                    .{open_elem.tag_name},
                ),
                .file_path = self.file_path,
                .start_line = loc.line,
                .start_col = loc.col,
                .end_line = loc.line,
                .end_col = loc.col + @as(u32, @intCast(open_elem.tag_name.len)) + 1, // +1 for <
            });
        }

        return ast;
    }

    fn parseNode(self: *Parser, ast: *Ast) !u32 {
        return switch (self.current.kind) {
            .script_start => try self.parseScript(ast),
            .style_start => try self.parseStyle(ast),
            .script_content, .style_content => {
                self.advance();
                return Node.NONE;
            },
            .text => try self.parseText(ast),
            .lt => try self.parseElement(ast),
            .lt_slash => {
                try self.parseClosingTag();
                return Node.NONE;
            },
            .lbrace => try self.parseExpression(ast),
            .comment => try self.parseComment(ast),
            else => {
                self.advance();
                return Node.NONE;
            },
        };
    }

    fn parseScript(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
        const tag_end = self.current.end;

        // Extract lang and context from <script ...> tag
        const attrs = parseScriptTagAttrs(self.source[start..tag_end]);

        self.advance(); // consume script_start

        var content_start: u32 = self.current.start;
        var content_end: u32 = self.current.start;

        if (self.current.kind == .script_content) {
            content_start = self.current.start;
            content_end = self.current.end;
            self.advance();
        }

        // Skip script_end
        if (self.current.kind == .script_end) {
            self.advance();
        }

        const data_idx: u32 = @intCast(ast.scripts.items.len);
        try ast.scripts.append(self.allocator, .{
            .content_start = content_start,
            .content_end = content_end,
            .lang = attrs.lang,
            .context = attrs.context,
            .generics = attrs.generics,
        });

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .script,
            .start = start,
            .end = content_end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = data_idx,
        });

        return node_idx;
    }

    fn parseStyle(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
        self.advance(); // consume style_start

        var content_start: u32 = self.current.start;
        var content_end: u32 = self.current.start;

        if (self.current.kind == .style_content) {
            content_start = self.current.start;
            content_end = self.current.end;
            self.advance();
        }

        if (self.current.kind == .style_end) {
            self.advance();
        }

        const data_idx: u32 = @intCast(ast.styles.items.len);
        try ast.styles.append(self.allocator, .{
            .content_start = content_start,
            .content_end = content_end,
            .lang = null,
        });

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .style,
            .start = start,
            .end = content_end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = data_idx,
        });

        return node_idx;
    }

    fn parseText(self: *Parser, ast: *Ast) !u32 {
        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .text,
            .start = self.current.start,
            .end = self.current.end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });
        self.advance();
        return node_idx;
    }

    fn parseElement(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
        self.advance(); // consume <

        // Get tag name
        if (self.current.kind != .identifier) {
            return Node.NONE;
        }

        const tag_name_start = self.current.start;
        var tag_name_end = self.current.end;
        const initial_name = self.current.slice(self.source);
        self.advance();

        // Handle svelte: special tags (svelte:head, svelte:body, etc.)
        // These are parsed as identifier + colon + identifier
        if (std.mem.eql(u8, initial_name, "svelte") and self.current.kind == .colon) {
            self.advance(); // consume :
            if (self.current.kind == .identifier) {
                tag_name_end = self.current.end;
                self.advance();
            }
        }

        // Note: Dotted component names (Select.Root, Tooltip.Content) are now handled
        // by the lexer, which produces a single identifier token for the full dotted name.

        const tag_name = self.source[tag_name_start..tag_name_end];
        const is_component = (tag_name.len > 0 and tag_name[0] >= 'A' and tag_name[0] <= 'Z') or
            std.mem.startsWith(u8, tag_name, "svelte:");

        // Handle generic type parameters: <Component<T> /> or <Component<T, U> />
        if (is_component and self.current.kind == .lt) {
            self.skipGenericParams();
        }

        // Parse attributes
        const attrs_start: u32 = @intCast(ast.attributes.items.len);
        while (self.current.kind != .gt and
            self.current.kind != .slash_gt and
            self.current.kind != .eof)
        {
            // Handle expressions in attribute position: {...expr} spreads and {ident} shorthand
            if (self.current.kind == .lbrace) {
                const expr_start = self.current.start;
                const expr_end = findMatchingBrace(self.source, expr_start);
                const expr_content = self.source[expr_start + 1 .. expr_end - 1];

                // Check if this is a spread {...foo} or shorthand {foo}
                // Spread has ... prefix, shorthand is just an identifier
                const is_spread = std.mem.startsWith(u8, std.mem.trim(u8, expr_content, " \t\n\r"), "...");

                if (is_spread) {
                    // Spread attribute: {...props} or {...toolCallProps(...)}
                    // Store with name="..." and value containing the spread expression (without ...)
                    const spread_expr = std.mem.trim(u8, expr_content[3..], " \t\n\r");
                    try ast.attributes.append(self.allocator, .{
                        .name = "...",
                        .value = spread_expr,
                        .start = expr_start,
                        .end = expr_end,
                    });
                    self.lexer.pos = expr_end;
                    self.advance();
                    continue;
                }

                // Shorthand syntax: {foo} is equivalent to foo={foo}
                // Check if the content is a simple identifier
                const trimmed = std.mem.trim(u8, expr_content, " \t\n\r");
                const is_simple_ident = blk: {
                    if (trimmed.len == 0) break :blk false;
                    // First char must be letter or underscore
                    if (!std.ascii.isAlphabetic(trimmed[0]) and trimmed[0] != '_') break :blk false;
                    // Rest must be alphanumeric or underscore
                    for (trimmed[1..]) |c| {
                        if (!std.ascii.isAlphanumeric(c) and c != '_') break :blk false;
                    }
                    break :blk true;
                };

                if (is_simple_ident) {
                    // Shorthand: {foo} → attribute name=foo, value=null
                    // The helper emitComponentPropsValidation will detect this via source inspection
                    try ast.attributes.append(self.allocator, .{
                        .name = trimmed,
                        .value = null,
                        .start = expr_start,
                        .end = expr_end,
                    });
                }

                // Reset lexer position to after the expression
                self.lexer.pos = expr_end;
                self.advance();
                continue;
            }

            if (self.current.kind == .identifier) {
                const attr_start = self.current.start;
                var attr_name_end = self.current.end;
                self.advance();

                // Handle Svelte directives (class:, on:, bind:, use:, style:, etc.)
                // These have the form name:modifier or name:modifier|options
                // Special cases where the lexer can't parse the modifier:
                // - style:--custom-property for CSS custom properties
                // - class:!classname for Tailwind's important modifier
                if (self.current.kind == .colon) {
                    const first_ident = self.source[attr_start..attr_name_end];
                    if (std.mem.eql(u8, first_ident, "class")) {
                        // class: directive - scan for full Tailwind/CSS class name FIRST
                        // before checking for identifier token, because the lexer can't
                        // parse class names with special characters like dots or brackets.
                        // This must come before the generic identifier check below.
                    } else if (self.peek().kind == .identifier) {
                        self.advance(); // consume :
                        attr_name_end = self.current.end;
                        self.advance(); // consume modifier name
                        // Handle |modifier options (e.g., in:fly|global, transition:fade|local)
                        while (self.current.kind == .pipe and self.peek().kind == .identifier) {
                            self.advance(); // consume |
                            attr_name_end = self.current.end;
                            self.advance(); // consume modifier option name
                        }
                    }
                    if (std.mem.eql(u8, first_ident, "style")) {
                        // style: directive - check for CSS custom property (--name)
                        // The lexer can't parse -- as part of an identifier, so we scan manually.
                        // After the colon, check if the source has -- indicating a CSS custom property.
                        const colon_pos = self.current.start;
                        // Look past the colon for --
                        if (colon_pos + 2 < self.source.len and
                            self.source[colon_pos + 1] == '-' and
                            self.source[colon_pos + 2] == '-')
                        {
                            var css_prop_end: usize = colon_pos + 3;
                            // CSS custom properties: -- followed by alphanumeric, -, _
                            while (css_prop_end < self.source.len) {
                                const c = self.source[css_prop_end];
                                if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_') {
                                    css_prop_end += 1;
                                } else {
                                    break;
                                }
                            }
                            attr_name_end = @intCast(css_prop_end);
                            self.lexer.pos = @intCast(css_prop_end);
                            self.advance();
                        } else {
                            self.advance(); // just consume the colon
                        }
                    } else if (std.mem.eql(u8, first_ident, "class")) {
                        // class: directive - scan for full Tailwind/CSS class name
                        // The lexer can't parse class names with special characters like:
                        // - Tailwind's ! important modifier: class:!hidden
                        // - Tailwind arbitrary values with dots: class:pb-1.5, class:text-[1.5rem]
                        // - Tailwind arbitrary values with brackets: class:bg-[#fff]
                        // We scan manually from the colon to capture the full class name.
                        const colon_pos = self.current.start;
                        var class_end: usize = colon_pos + 1;
                        // Skip optional ! prefix (Tailwind important modifier)
                        if (class_end < self.source.len and self.source[class_end] == '!') {
                            class_end += 1;
                        }
                        // Tailwind class names: alphanumeric, -, _, ., [, ], /, :, #, %, (, )
                        // Stop at = (value start), > (tag end), space, or { (expression)
                        var bracket_depth: u32 = 0;
                        while (class_end < self.source.len) {
                            const c = self.source[class_end];
                            if (c == '[') {
                                bracket_depth += 1;
                                class_end += 1;
                            } else if (c == ']') {
                                if (bracket_depth > 0) bracket_depth -= 1;
                                class_end += 1;
                            } else if (bracket_depth > 0) {
                                // Inside brackets, allow almost anything except unbalanced brackets
                                class_end += 1;
                            } else if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '/' or c == ':' or c == '#' or c == '%') {
                                class_end += 1;
                            } else {
                                break;
                            }
                        }
                        // Only update if we actually scanned something past the colon
                        if (class_end > colon_pos + 1) {
                            attr_name_end = @intCast(class_end);
                            self.lexer.pos = @intCast(class_end);
                            self.advance();
                        } else {
                            self.advance(); // just consume the colon
                        }
                    }
                }

                const attr_name = self.source[attr_start..attr_name_end];

                var attr_value: ?[]const u8 = null;
                if (self.current.kind == .eq) {
                    self.advance(); // consume =
                    if (self.current.kind == .string_double or self.current.kind == .string_single) {
                        // Extract value without quotes
                        const raw = self.current.slice(self.source);
                        if (raw.len >= 2) {
                            attr_value = raw[1 .. raw.len - 1];
                        }
                        self.advance();
                    } else if (self.current.kind == .lbrace) {
                        // Expression value {expr} - count braces manually in source
                        // because the lexer can't properly handle JavaScript inside templates
                        // (e.g., apostrophes in comments get misinterpreted as strings)
                        const expr_start = self.current.start;
                        const expr_end = findMatchingBrace(self.source, expr_start);

                        // Reset lexer position to after the expression
                        // We can't use token-by-token advancement because the lexer
                        // may misparse JavaScript (e.g., apostrophes in comments)
                        self.lexer.pos = expr_end;
                        self.advance(); // Get next token after the expression

                        attr_value = self.source[expr_start..expr_end];
                    }
                }

                try ast.attributes.append(self.allocator, .{
                    .name = attr_name,
                    .value = attr_value,
                    .start = attr_start,
                    .end = self.current.start,
                });
            } else {
                self.advance();
            }
        }
        const attrs_end: u32 = @intCast(ast.attributes.items.len);

        const is_self_closing = self.current.kind == .slash_gt;
        self.advance(); // consume > or />

        const data_idx: u32 = @intCast(ast.elements.items.len);
        try ast.elements.append(self.allocator, .{
            .tag_name = tag_name,
            .is_self_closing = is_self_closing,
            .is_component = is_component,
            .attrs_start = attrs_start,
            .attrs_end = attrs_end,
        });

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = if (is_component) .component else .element,
            .start = start,
            .end = self.current.start,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = data_idx,
        });

        // Track open elements for unclosed tag detection
        // Don't track self-closing elements, void elements, or components
        if (!is_self_closing and !is_component and !isVoidElement(tag_name)) {
            try self.element_stack.append(self.allocator, .{
                .tag_name = tag_name,
                .start = start,
            });
        }

        return node_idx;
    }

    /// Parse closing tag </tagname> and pop from element stack
    fn parseClosingTag(self: *Parser) !void {
        self.advance(); // consume </

        // Get tag name
        if (self.current.kind != .identifier) {
            // Skip malformed closing tag
            while (self.current.kind != .gt and self.current.kind != .eof) {
                self.advance();
            }
            if (self.current.kind == .gt) self.advance();
            return;
        }

        const tag_name_start = self.current.start;
        var tag_name_end = self.current.end;
        const initial_name = self.current.slice(self.source);
        self.advance();

        // Handle svelte: special tags
        if (std.mem.eql(u8, initial_name, "svelte") and self.current.kind == .colon) {
            self.advance(); // consume :
            if (self.current.kind == .identifier) {
                tag_name_end = self.current.end;
                self.advance();
            }
        }

        // Note: Dotted component names are now handled by the lexer.

        const tag_name = self.source[tag_name_start..tag_name_end];

        // Skip to >
        while (self.current.kind != .gt and self.current.kind != .eof) {
            self.advance();
        }
        if (self.current.kind == .gt) self.advance();

        // Pop matching element from stack
        // Search from top of stack (most recent) to find matching tag
        var i = self.element_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.element_stack.items[i].tag_name, tag_name)) {
                _ = self.element_stack.orderedRemove(i);
                return;
            }
        }
        // No matching opening tag found - that's a different error (unmatched close tag)
        // For now we just ignore it
    }

    fn parseExpression(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
        self.advance(); // consume {

        // Check for block/tag keywords
        if (self.current.kind == .hash) {
            return self.parseBlockStart(ast, start);
        } else if (self.current.kind == .at) {
            return self.parseTagExpression(ast, start);
        } else if (self.current.kind == .slash) {
            return self.parseBlockEnd(ast, start);
        } else if (self.current.kind == .colon) {
            return self.parseBlockContinuation(ast, start);
        }

        // Regular expression - find matching }
        var depth: u32 = 1;
        while (self.current.kind != .eof and depth > 0) {
            if (self.current.kind == .lbrace) depth += 1;
            if (self.current.kind == .rbrace) depth -= 1;
            if (depth > 0) self.advance();
        }

        const end = self.current.end;
        if (self.current.kind == .rbrace) self.advance();

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .expression,
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        return node_idx;
    }

    /// Parse {#if}, {#each}, {#await}, {#key}, {#snippet}
    fn parseBlockStart(self: *Parser, ast: *Ast, start: u32) !u32 {
        self.advance(); // consume #

        const kind: NodeKind = if (self.current.kind == .identifier) blk: {
            const name = self.current.slice(self.source);
            break :blk if (std.mem.eql(u8, name, "if"))
                .if_block
            else if (std.mem.eql(u8, name, "each"))
                .each_block
            else if (std.mem.eql(u8, name, "await"))
                .await_block
            else if (std.mem.eql(u8, name, "key"))
                .key_block
            else if (std.mem.eql(u8, name, "snippet"))
                .snippet
            else
                .expression;
        } else .expression;

        // Skip to closing brace
        self.skipToClosingBrace();
        const end = self.current.end;
        if (self.current.kind == .rbrace) self.advance();

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        return node_idx;
    }

    /// Parse {@render}, {@html}, {@const}, {@debug}
    fn parseTagExpression(self: *Parser, ast: *Ast, start: u32) !u32 {
        self.advance(); // consume @

        const kind: NodeKind = if (self.current.kind == .identifier) blk: {
            const name = self.current.slice(self.source);
            break :blk if (std.mem.eql(u8, name, "render"))
                .render
            else if (std.mem.eql(u8, name, "html"))
                .html
            else if (std.mem.eql(u8, name, "const"))
                .const_tag
            else if (std.mem.eql(u8, name, "debug"))
                .debug_tag
            else
                .expression;
        } else .expression;

        // Skip to closing brace
        self.skipToClosingBrace();
        const end = self.current.end;
        if (self.current.kind == .rbrace) self.advance();

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        return node_idx;
    }

    /// Parse {/if}, {/each}, {/await}, {/key}, {/snippet}
    fn parseBlockEnd(self: *Parser, ast: *Ast, start: u32) !u32 {
        self.advance(); // consume /

        // Block ends just mark position, use expression kind since they close blocks
        self.skipToClosingBrace();
        const end = self.current.end;
        if (self.current.kind == .rbrace) self.advance();

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .expression, // Block end markers
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        return node_idx;
    }

    /// Parse {:else}, {:then}, {:catch}
    fn parseBlockContinuation(self: *Parser, ast: *Ast, start: u32) !u32 {
        self.advance(); // consume :

        const kind: NodeKind = if (self.current.kind == .identifier) blk: {
            const name = self.current.slice(self.source);
            break :blk if (std.mem.eql(u8, name, "else"))
                .else_block
            else if (std.mem.eql(u8, name, "then"))
                .then_block
            else if (std.mem.eql(u8, name, "catch"))
                .catch_block
            else
                .expression;
        } else .expression;

        self.skipToClosingBrace();
        const end = self.current.end;
        if (self.current.kind == .rbrace) self.advance();

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = 0,
        });

        return node_idx;
    }

    fn skipToClosingBrace(self: *Parser) void {
        var depth: u32 = 1;
        while (self.current.kind != .eof and depth > 0) {
            if (self.current.kind == .lbrace) depth += 1;
            if (self.current.kind == .rbrace) depth -= 1;
            if (depth > 0) self.advance();
        }
    }

    /// Skip generic type parameters like <T> or <T, U extends Foo>
    fn skipGenericParams(self: *Parser) void {
        if (self.current.kind != .lt) return;
        self.advance(); // consume <

        var depth: u32 = 1;
        while (self.current.kind != .eof and depth > 0) {
            if (self.current.kind == .lt) depth += 1;
            if (self.current.kind == .gt) depth -= 1;
            if (depth > 0) self.advance();
        }
        if (self.current.kind == .gt) self.advance();
    }

    fn parseComment(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
        const end = self.current.end;

        // Extract content between <!-- and -->
        const token_slice = self.source[start..end];
        var content: []const u8 = "";
        if (token_slice.len >= 7) { // "<!--" + "-->"
            content = std.mem.trim(u8, token_slice[4 .. token_slice.len - 3], " \t\n\r");
        }

        // Parse svelte-ignore codes if present
        const ignore_codes_start: u32 = @intCast(ast.ignore_codes.items.len);
        try parseSvelteIgnoreCodes(self.allocator, content, &ast.ignore_codes);
        const ignore_codes_end: u32 = @intCast(ast.ignore_codes.items.len);

        const data_idx: u32 = @intCast(ast.comments.items.len);
        try ast.comments.append(self.allocator, .{
            .content = content,
            .ignore_codes_start = ignore_codes_start,
            .ignore_codes_end = ignore_codes_end,
        });

        const node_idx: u32 = @intCast(ast.nodes.items.len);
        try ast.nodes.append(self.allocator, .{
            .kind = .comment,
            .start = start,
            .end = end,
            .first_child = Node.NONE,
            .next_sibling = Node.NONE,
            .data = data_idx,
        });

        self.advance(); // consume comment token

        return node_idx;
    }

    fn advance(self: *Parser) void {
        self.current = self.lexer.next();
    }

    fn peek(self: *Parser) Token {
        // Save lexer state
        const saved_pos = self.lexer.pos;
        const saved_in_script = self.lexer.in_script;
        const saved_in_style = self.lexer.in_style;

        const next_token = self.lexer.next();

        // Restore lexer state
        self.lexer.pos = saved_pos;
        self.lexer.in_script = saved_in_script;
        self.lexer.in_style = saved_in_style;

        return next_token;
    }
};

const ScriptTagAttrs = struct {
    lang: ?[]const u8,
    context: ?[]const u8,
    generics: ?[]const u8,
};

/// Parse svelte-ignore codes from a comment's content.
/// Format: "svelte-ignore code1 code2 code3"
fn parseSvelteIgnoreCodes(
    allocator: std.mem.Allocator,
    content: []const u8,
    codes: *std.ArrayList([]const u8),
) !void {
    const prefix = "svelte-ignore";

    // Check if content starts with "svelte-ignore"
    const trimmed = std.mem.trimLeft(u8, content, " \t");
    if (!std.mem.startsWith(u8, trimmed, prefix)) return;

    // Skip the prefix and parse codes
    const rest = trimmed[prefix.len..];
    if (rest.len == 0) return;

    // Must be followed by whitespace
    if (!std.ascii.isWhitespace(rest[0])) return;

    // Parse space-separated codes
    var iter = std.mem.tokenizeAny(u8, rest, " \t\n\r");
    while (iter.next()) |code| {
        try codes.append(allocator, code);
    }
}

fn parseScriptTagAttrs(tag: []const u8) ScriptTagAttrs {
    var i: usize = 0;
    var lang: ?[]const u8 = null;
    var context: ?[]const u8 = null;
    var generics: ?[]const u8 = null;

    // Skip "<script"
    while (i < tag.len and tag[i] != '>' and !std.ascii.isWhitespace(tag[i])) : (i += 1) {}

    while (i < tag.len and tag[i] != '>') {
        // Skip whitespace
        while (i < tag.len and std.ascii.isWhitespace(tag[i])) : (i += 1) {}
        if (i >= tag.len or tag[i] == '>') break;

        const name_start = i;
        while (i < tag.len and tag[i] != '=' and !std.ascii.isWhitespace(tag[i]) and tag[i] != '>') : (i += 1) {}
        const name = tag[name_start..i];

        // Skip whitespace
        while (i < tag.len and std.ascii.isWhitespace(tag[i])) : (i += 1) {}

        var value: ?[]const u8 = null;
        if (i < tag.len and tag[i] == '=') {
            i += 1;
            while (i < tag.len and std.ascii.isWhitespace(tag[i])) : (i += 1) {}

            if (i < tag.len and (tag[i] == '"' or tag[i] == '\'')) {
                const quote = tag[i];
                i += 1;
                const v_start = i;
                while (i < tag.len and tag[i] != quote) : (i += 1) {}
                const v_end = i;
                if (i < tag.len and tag[i] == quote) i += 1;
                value = tag[v_start..v_end];
            } else {
                const v_start = i;
                while (i < tag.len and !std.ascii.isWhitespace(tag[i]) and tag[i] != '>') : (i += 1) {}
                value = tag[v_start..i];
            }
        }

        if (std.mem.eql(u8, name, "lang")) {
            lang = value;
        } else if (std.mem.eql(u8, name, "context")) {
            context = value;
        } else if (std.mem.eql(u8, name, "module") and value == null) {
            // Svelte 5 bare `module` attribute: <script module>
            context = "module";
        } else if (std.mem.eql(u8, name, "generics")) {
            generics = value;
        }
    }

    return .{ .lang = lang, .context = context, .generics = generics };
}

/// Convert byte offset to line and column (1-indexed).
fn computeLineCol(source: []const u8, pos: u32) struct { line: u32, col: u32 } {
    var line: u32 = 1;
    var col: u32 = 1;

    for (source[0..@min(pos, source.len)]) |c| {
        if (c == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .col = col };
}

test "parse simple svelte" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script>
        \\  let count = 0;
        \\</script>
        \\
        \\<button>Click</button>
    ;

    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expect(ast.nodes.items.len > 0);
    try std.testing.expect(ast.scripts.items.len == 1);
}

test "parse script tag attributes" {
    // Test basic script tag
    {
        const attrs = parseScriptTagAttrs("<script>");
        try std.testing.expect(attrs.lang == null);
        try std.testing.expect(attrs.context == null);
    }
    // Test lang="ts"
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\">");
        try std.testing.expectEqualStrings("ts", attrs.lang.?);
        try std.testing.expect(attrs.context == null);
    }
    // Test both attributes
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\" context=\"module\">");
        try std.testing.expectEqualStrings("ts", attrs.lang.?);
        try std.testing.expectEqualStrings("module", attrs.context.?);
    }
    // Test context="module" only
    {
        const attrs = parseScriptTagAttrs("<script context=\"module\">");
        try std.testing.expect(attrs.lang == null);
        try std.testing.expectEqualStrings("module", attrs.context.?);
    }
    // Test single quotes
    {
        const attrs = parseScriptTagAttrs("<script lang='ts'>");
        try std.testing.expectEqualStrings("ts", attrs.lang.?);
    }
    // Test Svelte 5 bare module attribute
    {
        const attrs = parseScriptTagAttrs("<script module>");
        try std.testing.expect(attrs.lang == null);
        try std.testing.expectEqualStrings("module", attrs.context.?);
    }
    // Test Svelte 5 bare module with lang
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\" module>");
        try std.testing.expectEqualStrings("ts", attrs.lang.?);
        try std.testing.expectEqualStrings("module", attrs.context.?);
    }
    // Test generics attribute
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\" generics=\"T\">");
        try std.testing.expectEqualStrings("ts", attrs.lang.?);
        try std.testing.expectEqualStrings("T", attrs.generics.?);
    }
    // Test generics with extends constraint
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\" generics=\"T extends SomeType\">");
        try std.testing.expectEqualStrings("T extends SomeType", attrs.generics.?);
    }
    // Test multiple generic parameters
    {
        const attrs = parseScriptTagAttrs("<script lang=\"ts\" generics=\"T, U extends Record<string, T>\">");
        try std.testing.expectEqualStrings("T, U extends Record<string, T>", attrs.generics.?);
    }
}

test "parse typescript script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count: number = 0;
        \\</script>
    ;

    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), ast.scripts.items.len);
    try std.testing.expectEqualStrings("ts", ast.scripts.items[0].lang.?);
}

test "parse script with generics attribute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts" module>
        \\  export type Item<T> = { value: T };
        \\</script>
        \\
        \\<script lang="ts" generics="T">
        \\  let items: T[] = [];
        \\</script>
    ;

    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 2), ast.scripts.items.len);
    // Module script
    try std.testing.expectEqualStrings("module", ast.scripts.items[0].context.?);
    try std.testing.expect(ast.scripts.items[0].generics == null);
    // Instance script with generics
    try std.testing.expect(ast.scripts.items[1].context == null);
    try std.testing.expectEqualStrings("T", ast.scripts.items[1].generics.?);
}

test "parse module script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script context="module">
        \\  export const VERSION = "1.0";
        \\</script>
        \\<script>
        \\  let count = 0;
        \\</script>
    ;

    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 2), ast.scripts.items.len);
    try std.testing.expectEqualStrings("module", ast.scripts.items[0].context.?);
    try std.testing.expect(ast.scripts.items[1].context == null);
}

test "parse svelte-ignore comment" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<!-- svelte-ignore a11y_missing_attribute a11y_autofocus -->\n<img src=\"test.png\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // Should have parsed the comment
    try std.testing.expectEqual(@as(usize, 1), ast.comments.items.len);

    // Should have extracted the ignore codes
    try std.testing.expectEqual(@as(usize, 2), ast.ignore_codes.items.len);
    try std.testing.expectEqualStrings("a11y_missing_attribute", ast.ignore_codes.items[0]);
    try std.testing.expectEqualStrings("a11y_autofocus", ast.ignore_codes.items[1]);

    // Comment data should reference the correct range in ignore_codes
    const comment = ast.comments.items[0];
    try std.testing.expectEqual(@as(u32, 0), comment.ignore_codes_start);
    try std.testing.expectEqual(@as(u32, 2), comment.ignore_codes_end);
}

test "parse regular comment without svelte-ignore" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<!-- Just a regular comment -->\n<div>Hello</div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // Should have parsed the comment
    try std.testing.expectEqual(@as(usize, 1), ast.comments.items.len);

    // Should have no ignore codes
    try std.testing.expectEqual(@as(usize, 0), ast.ignore_codes.items.len);

    // Comment data should have empty ignore_codes range
    const comment = ast.comments.items[0];
    try std.testing.expectEqual(@as(u32, 0), comment.ignore_codes_start);
    try std.testing.expectEqual(@as(u32, 0), comment.ignore_codes_end);
}

test "parse snippet block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#snippet greeting(name)}<p>Hello {name}!</p>{/snippet}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // Find snippet node
    var found_snippet = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) {
            found_snippet = true;
            break;
        }
    }
    try std.testing.expect(found_snippet);
}

test "parse render tag" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{@render greeting('world')}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_render = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .render) {
            found_render = true;
            break;
        }
    }
    try std.testing.expect(found_render);
}

test "parse html tag" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{@html '<b>bold</b>'}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_html = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .html) {
            found_html = true;
            break;
        }
    }
    try std.testing.expect(found_html);
}

test "parse const tag" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#each items as item}{@const doubled = item * 2}{doubled}{/each}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_const = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .const_tag) {
            found_const = true;
            break;
        }
    }
    try std.testing.expect(found_const);
}

test "parse debug tag" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{@debug foo, bar}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_debug = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .debug_tag) {
            found_debug = true;
            break;
        }
    }
    try std.testing.expect(found_debug);
}

test "parse if-else block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#if condition}<p>Yes</p>{:else}<p>No</p>{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_if = false;
    var found_else = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .if_block) found_if = true;
        if (node.kind == .else_block) found_else = true;
    }
    try std.testing.expect(found_if);
    try std.testing.expect(found_else);
}

test "parse await block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#await promise}<p>Loading...</p>{:then value}<p>{value}</p>{:catch error}<p>{error}</p>{/await}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_await = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .await_block) {
            found_await = true;
            break;
        }
    }
    try std.testing.expect(found_await);
}

test "parse key block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#key value}<Component />{/key}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_key = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .key_block) {
            found_key = true;
            break;
        }
    }
    try std.testing.expect(found_key);
}

test "parse component with generics" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<Table<User> items={users} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_component = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .component) {
            found_component = true;
            const elem = ast.elements.items[node.data];
            try std.testing.expectEqualStrings("Table", elem.tag_name);
            try std.testing.expect(elem.is_self_closing);
            break;
        }
    }
    try std.testing.expect(found_component);
}

// ============================================================================
// Special svelte: tag tests
// ============================================================================

test "parse svelte:head" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:head><title>My App</title></svelte:head>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_head = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:head")) {
                found_svelte_head = true;
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_head);
}

test "parse svelte:body with event handler" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:body on:keydown={handleKeydown} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_body = false;
    var found_on_keydown = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:body")) {
                found_svelte_body = true;
                // Check attributes
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "on:keydown")) {
                        found_on_keydown = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_body);
    try std.testing.expect(found_on_keydown);
}

test "parse svelte:window with bind" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:window bind:innerWidth={width} on:resize={handleResize} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_window = false;
    var found_bind = false;
    var found_on_resize = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:window")) {
                found_svelte_window = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "bind:innerWidth")) {
                        found_bind = true;
                    }
                    if (std.mem.eql(u8, attr.name, "on:resize")) {
                        found_on_resize = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_window);
    try std.testing.expect(found_bind);
    try std.testing.expect(found_on_resize);
}

test "parse svelte:document" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:document on:visibilitychange={handleVisibility} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_document = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:document")) {
                found_svelte_document = true;
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_document);
}

test "parse svelte:element with dynamic tag" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:element this={tag}>Content</svelte:element>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_element = false;
    var found_this_attr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:element")) {
                found_svelte_element = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "this")) {
                        found_this_attr = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_element);
    try std.testing.expect(found_this_attr);
}

test "parse svelte:component with dynamic component" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:component this={MyComponent} prop={value} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_component = false;
    var found_this_attr = false;
    var found_prop_attr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:component")) {
                found_svelte_component = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "this")) {
                        found_this_attr = true;
                    }
                    if (std.mem.eql(u8, attr.name, "prop")) {
                        found_prop_attr = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_component);
    try std.testing.expect(found_this_attr);
    try std.testing.expect(found_prop_attr);
}

test "parse svelte:self for recursive component" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#if depth > 0}<svelte:self depth={depth - 1} />{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_self = false;
    var found_depth_attr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:self")) {
                found_svelte_self = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "depth")) {
                        found_depth_attr = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_self);
    try std.testing.expect(found_depth_attr);
}

test "parse svelte:fragment for slot content" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<Component>
        \\  <svelte:fragment slot="header">
        \\    <h1>Title</h1>
        \\  </svelte:fragment>
        \\</Component>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_fragment = false;
    var found_slot_attr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:fragment")) {
                found_svelte_fragment = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "slot")) {
                        found_slot_attr = true;
                        try std.testing.expectEqualStrings("header", attr.value.?);
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_fragment);
    try std.testing.expect(found_slot_attr);
}

test "parse svelte:options" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:options runes={true} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_options = false;
    var found_runes_attr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:options")) {
                found_svelte_options = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "runes")) {
                        found_runes_attr = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_options);
    try std.testing.expect(found_runes_attr);
}

test "parse svelte:options with immutable and accessors" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:options immutable accessors />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_svelte_options = false;
    var found_immutable = false;
    var found_accessors = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "svelte:options")) {
                found_svelte_options = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "immutable")) {
                        found_immutable = true;
                    }
                    if (std.mem.eql(u8, attr.name, "accessors")) {
                        found_accessors = true;
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_svelte_options);
    try std.testing.expect(found_immutable);
    try std.testing.expect(found_accessors);
}

// ============================================================================
// Void element tests (self-closing HTML elements)
// ============================================================================

test "parse void element br" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<p>Line 1<br>Line 2</p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_br = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "br")) {
                found_br = true;
                break;
            }
        }
    }
    try std.testing.expect(found_br);
}

test "parse void element hr" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><hr></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_hr = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "hr")) {
                found_hr = true;
                break;
            }
        }
    }
    try std.testing.expect(found_hr);
}

test "parse void element img with attributes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<img src=\"photo.jpg\" alt=\"A photo\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_img = false;
    var found_src = false;
    var found_alt = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "img")) {
                found_img = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "src")) {
                        found_src = true;
                        try std.testing.expectEqualStrings("photo.jpg", attr.value.?);
                    }
                    if (std.mem.eql(u8, attr.name, "alt")) {
                        found_alt = true;
                        try std.testing.expectEqualStrings("A photo", attr.value.?);
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_img);
    try std.testing.expect(found_src);
    try std.testing.expect(found_alt);
}

test "parse void element input with type and value" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<input type=\"text\" value=\"hello\">";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_input = false;
    var found_type = false;
    var found_value = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "input")) {
                found_input = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "type")) {
                        found_type = true;
                        try std.testing.expectEqualStrings("text", attr.value.?);
                    }
                    if (std.mem.eql(u8, attr.name, "value")) {
                        found_value = true;
                        try std.testing.expectEqualStrings("hello", attr.value.?);
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_input);
    try std.testing.expect(found_type);
    try std.testing.expect(found_value);
}

test "parse expression attribute value" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<Icon icon={Map} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_icon = false;
    var found_attr = false;
    for (ast.nodes.items) |node| {
        // Component nodes also use elements array
        if (node.kind == .element or node.kind == .component) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "Icon")) {
                found_icon = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "icon")) {
                        found_attr = true;
                        try std.testing.expectEqualStrings("{Map}", attr.value.?);
                    }
                }
            }
        }
    }
    try std.testing.expect(found_icon);
    try std.testing.expect(found_attr);
}

test "parse void element meta" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:head><meta name=\"description\" content=\"A test page\"></svelte:head>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_meta = false;
    var found_name = false;
    var found_content = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "meta")) {
                found_meta = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "name")) {
                        found_name = true;
                        try std.testing.expectEqualStrings("description", attr.value.?);
                    }
                    if (std.mem.eql(u8, attr.name, "content")) {
                        found_content = true;
                        try std.testing.expectEqualStrings("A test page", attr.value.?);
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_meta);
    try std.testing.expect(found_name);
    try std.testing.expect(found_content);
}

test "parse void element link" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<svelte:head><link rel=\"stylesheet\" href=\"styles.css\"></svelte:head>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_link = false;
    var found_rel = false;
    var found_href = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "link")) {
                found_link = true;
                for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
                    if (std.mem.eql(u8, attr.name, "rel")) {
                        found_rel = true;
                        try std.testing.expectEqualStrings("stylesheet", attr.value.?);
                    }
                    if (std.mem.eql(u8, attr.name, "href")) {
                        found_href = true;
                        try std.testing.expectEqualStrings("styles.css", attr.value.?);
                    }
                }
                break;
            }
        }
    }
    try std.testing.expect(found_link);
    try std.testing.expect(found_rel);
    try std.testing.expect(found_href);
}

test "parse void element with self-closing syntax" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<input type=\"checkbox\" />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_input = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "input")) {
                found_input = true;
                try std.testing.expect(elem.is_self_closing);
                break;
            }
        }
    }
    try std.testing.expect(found_input);
}

test "parse multiple void elements in sequence" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<br><hr><br>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var br_count: u32 = 0;
    var hr_count: u32 = 0;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "br")) {
                br_count += 1;
            }
            if (std.mem.eql(u8, elem.tag_name, "hr")) {
                hr_count += 1;
            }
        }
    }
    try std.testing.expectEqual(@as(u32, 2), br_count);
    try std.testing.expectEqual(@as(u32, 1), hr_count);
}

// === Regex Literal Edge Cases ===
// These tests verify the parser handles expressions containing regex literals correctly.
// The parser doesn't interpret JS expressions deeply but must not be confused by regex syntax.

test "parse simple regex literal in expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{value.match(/test/)}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex with flags" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{value.replace(/test/gi, 'REPLACED')}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex with quantifier braces" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Regex {n,m} quantifiers should not confuse brace counting
    const source =
        \\{value.match(/\d{2,4}/)}
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex with character class containing special chars" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Character classes can contain } and ) without closing the expression
    const source = "{value.match(/[{}]+/)}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex with escaped slash" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\{value.match(/path\/to\/file/)}
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse const with regex match" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // @const with regex from svelte-check-rs issue #46
    const source = "{#if true}{@const [, label] = value.match(/^(.+?)\\s*\\(([^)]+)\\)$/) ?? [, value, ``]}{label}{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_const = false;
    var found_if = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .const_tag) found_const = true;
        if (node.kind == .if_block) found_if = true;
    }
    try std.testing.expect(found_const);
    try std.testing.expect(found_if);
}

test "parse const with regex test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{#if true}{@const matches = /rgba\\([^)]+\\)$/.test(color)}{matches}{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_const = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .const_tag) {
            found_const = true;
            break;
        }
    }
    try std.testing.expect(found_const);
}

test "parse snippet with const regex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\{#snippet tooltip({ x })}
        \\    {@const match = x.match(/test/)}
        \\    <span>{match}</span>
        \\{/snippet}
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_snippet = false;
    var found_const = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) found_snippet = true;
        if (node.kind == .const_tag) found_const = true;
    }
    try std.testing.expect(found_snippet);
    try std.testing.expect(found_const);
}

test "parse const arrow function with regex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Arrow function containing regex
    const source = "{#if true}{@const check = (s: string): boolean => /test/.test(s)}{check('x')}{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_const = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .const_tag) {
            found_const = true;
            break;
        }
    }
    try std.testing.expect(found_const);
}

test "parse const iife with regex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // IIFE containing regex
    const source = "{#if true}{@const result = (() => { return /test/.test(x); })()}{result}{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_const = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .const_tag) {
            found_const = true;
            break;
        }
    }
    try std.testing.expect(found_const);
}

test "parse division not confused with regex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Division should parse correctly (not as regex)
    const source = "{(a + b) / 2}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex in template literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Regex inside template literal
    const source = "{`result: ${/test/.test(x)}`}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex with colons" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Regex with colons should not confuse the parser
    const source =
        \\<script lang="ts">
        \\    const timeRegex = /\d{2}:\d{2}:\d{2}/;
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), ast.scripts.items.len);
}

test "parse regex in each block filter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\{#each items.filter(x => /test/.test(x)) as item}
        \\    <span>{item}</span>
        \\{/each}
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_each = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .each_block) {
            found_each = true;
            break;
        }
    }
    try std.testing.expect(found_each);
}

test "parse multiple regex in expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Multiple regex patterns in one expression
    const source = "{[/a/, /b/, /c/]}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex in ternary" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{true ? /yes/ : /no/}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse regex after logical operator" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "{false || /fallback/.test(value)}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_expression = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .expression) {
            found_expression = true;
            break;
        }
    }
    try std.testing.expect(found_expression);
}

test "parse complex snippet with multiple const and regex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Complex case from svelte-check-rs issue #46
    const source =
        \\<div class="parent">
        \\  {#snippet tooltip({ x, y_formatted }: { x: number; y_formatted: string })}
        \\    {@const [, y_label, y_unit] = y_label_full.match(/^(.+?)\s*\(([^)]+)\)$/) ??
        \\      [, y_label_full, ``]}
        \\    {@const segment = Object.entries(x_positions ?? {}).find(([, [start, end]]) =>
        \\      x >= start && x <= end
        \\    )}
        \\    <span>{y_label}: {y_formatted} {y_unit}</span>
        \\  {/snippet}
        \\</div>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var found_snippet = false;
    var const_count: u32 = 0;
    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) found_snippet = true;
        if (node.kind == .const_tag) const_count += 1;
    }
    try std.testing.expect(found_snippet);
    try std.testing.expectEqual(@as(u32, 2), const_count);
}

// ============================================================================
// Unclosed element detection tests
// ============================================================================

test "unclosed element detected" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div>\n\t<p></p>\n\t\n\t<div></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // Should have one parse error for unclosed <div>
    try std.testing.expectEqual(@as(usize, 1), ast.parse_errors.items.len);
    try std.testing.expect(std.mem.indexOf(u8, ast.parse_errors.items[0].message, "<div>") != null);
    try std.testing.expect(std.mem.indexOf(u8, ast.parse_errors.items[0].message, "left open") != null);
}

test "properly closed elements no error" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><p>Hello</p></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "self-closing element no error" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><img src=\"test.png\" /></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "void elements no closing tag needed" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><br><hr><input type=\"text\"></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "multiple unclosed elements" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><span><p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // Should have three parse errors
    try std.testing.expectEqual(@as(usize, 3), ast.parse_errors.items.len);
}

test "dotted component names should not cause false positives" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div><Component.Root>content</Component.Root></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    // No parse errors - dotted component names should be fully matched
    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "deeply nested dotted component names" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<Select.Root><Select.Trigger>Click</Select.Trigger></Select.Root>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "complex template literal in attribute doesn't cause false unclosed tag" {
    // Regression test: multiline template strings with JS comments in attribute expressions
    // The lexer can't properly parse JavaScript (e.g., apostrophes in comments like "doesn't"),
    // but the parser's findMatchingBrace + lexer position reset should handle this correctly.
    const source =
        \\<div
        \\    style={`max-height: ${x + 2}px; ${`
        \\        ${(() => {
        \\            if (position.rightAligned) {
        \\                // position.right is a CSS right offset (distance from viewport right edge)
        \\                // We need to check if there's enough space to the left of the anchor for the menu
        \\                const leftSpaceFromAnchor = viewportWidth - (position.right || 0)
        \\                if (leftSpaceFromAnchor >= menuWidth + 10) {
        \\                    // Enough space to the left of anchor for the menu
        \\                    return `left: auto; right: ${position.right}px; width: ${menuWidth}px;`
        \\                } else {
        \\                    // Not enough space, switch to left alignment
        \\                    const leftPos = Math.max(10, viewportWidth - menuWidth - 10)
        \\                    return `left: ${leftPos}px; width: ${menuWidth}px;`
        \\                }
        \\            } else {
        \\                // Left-aligned: ensure it doesn't go off right edge
        \\                const maxLeft = Math.max(10, viewportWidth - menuWidth - 10)
        \\                const leftPos = Math.min(position.left || 0, maxLeft)
        \\                return `left: ${leftPos}px; width: ${menuWidth}px;`
        \\            }
        \\        })()}
        \\    `}`}
        \\>
        \\    Content
        \\</div>
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);
}

test "transition directive with pipe modifiers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div in:fly|global={{ y: 50 }} transition:fade|local></div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    try std.testing.expectEqual(@as(usize, 0), ast.parse_errors.items.len);

    // Should have 1 element (div) with 2 attributes
    var div_found = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .element) {
            const elem = ast.elements.items[node.data];
            if (std.mem.eql(u8, elem.tag_name, "div")) {
                div_found = true;
                const attrs = ast.attributes.items[elem.attrs_start..elem.attrs_end];
                try std.testing.expectEqual(@as(usize, 2), attrs.len);
                try std.testing.expectEqualStrings("in:fly|global", attrs[0].name);
                try std.testing.expectEqualStrings("transition:fade|local", attrs[1].name);
            }
        }
    }
    try std.testing.expect(div_found);
}
