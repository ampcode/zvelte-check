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

pub const ElementData = struct {
    tag_name: []const u8,
    is_self_closing: bool,
    is_component: bool, // PascalCase or svelte:*
};

pub const Ast = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    file_path: []const u8,

    nodes: std.ArrayList(Node),
    scripts: std.ArrayList(ScriptData),
    styles: std.ArrayList(StyleData),
    elements: std.ArrayList(ElementData),

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8) Ast {
        return .{
            .allocator = allocator,
            .source = source,
            .file_path = file_path,
            .nodes = .empty,
            .scripts = .empty,
            .styles = .empty,
            .elements = .empty,
        };
    }

    // No deinit needed - arena handles cleanup

    pub fn runDiagnostics(self: *const Ast, allocator: std.mem.Allocator, diagnostics: *std.ArrayList(Diagnostic)) !void {
        // TODO: implement a11y and CSS diagnostics
        _ = self;
        _ = allocator;
        _ = diagnostics;
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
            else => {
                self.advance();
                return Node.NONE;
            },
        };
    }

    fn parseScript(self: *Parser, ast: *Ast) !u32 {
        const start = self.current.start;
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
            .lang = null, // TODO: parse from attributes
            .context = null,
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

        // Skip attributes for now
        while (self.current.kind != .gt and
            self.current.kind != .slash_gt and
            self.current.kind != .eof)
        {
            self.advance();
        }

        const is_self_closing = self.current.kind == .slash_gt;
        self.advance(); // consume > or />

        const data_idx: u32 = @intCast(ast.elements.items.len);
        try ast.elements.append(self.allocator, .{
            .tag_name = tag_name,
            .is_self_closing = is_self_closing,
            .is_component = is_component,
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

        // Find matching }
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

    fn advance(self: *Parser) void {
        self.current = self.lexer.next();
    }
};

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
