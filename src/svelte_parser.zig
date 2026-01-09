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
        };
    }

    // No deinit needed - arena handles cleanup

    pub fn runDiagnostics(self: *const Ast, allocator: std.mem.Allocator, diagnostics: *std.ArrayList(Diagnostic)) !void {
        try a11y.runDiagnostics(allocator, self, diagnostics);
        try css.runDiagnostics(allocator, self, diagnostics);
        try component.runDiagnostics(allocator, self, diagnostics);
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    current: Token,
    source: []const u8,
    file_path: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8) Parser {
        var lexer = Lexer.init(source, file_path);
        const first = lexer.next();
        return .{
            .allocator = allocator,
            .lexer = lexer,
            .current = first,
            .source = source,
            .file_path = file_path,
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

        const tag_name = self.current.slice(self.source);
        const is_component = tag_name.len > 0 and tag_name[0] >= 'A' and tag_name[0] <= 'Z';
        self.advance();

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
            if (self.current.kind == .identifier) {
                const attr_start = self.current.start;
                var attr_name_end = self.current.end;
                self.advance();

                // Handle Svelte directives (class:, on:, bind:, use:, etc.)
                // These have the form name:modifier or name:modifier|options
                if (self.current.kind == .colon and self.peek().kind == .identifier) {
                    self.advance(); // consume :
                    attr_name_end = self.current.end;
                    self.advance(); // consume modifier name
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
                        // Expression value {expr} - store as expression marker
                        const expr_start = self.current.start;
                        var depth: u32 = 0;
                        while (self.current.kind != .eof) {
                            if (self.current.kind == .lbrace) depth += 1;
                            if (self.current.kind == .rbrace) {
                                depth -= 1;
                                if (depth == 0) {
                                    self.advance();
                                    break;
                                }
                            }
                            self.advance();
                        }
                        attr_value = self.source[expr_start..self.current.start];
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

        return node_idx;
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
        }
    }

    return .{ .lang = lang, .context = context };
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
