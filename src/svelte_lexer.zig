//! Svelte lexer - tokenizes .svelte files
//!
//! Handles HTML-like markup, script blocks, style blocks, and Svelte-specific syntax.

const std = @import("std");

pub const TokenKind = enum(u8) {
    // Basic
    eof,
    invalid,
    whitespace,
    text,

    // Punctuation
    lt, // <
    gt, // >
    lt_slash, // </
    slash_gt, // />
    eq, // =
    lbrace, // {
    rbrace, // }
    colon, // :
    pipe, // |
    hash, // #
    at, // @
    slash, // /

    // Strings
    string_double, // "..."
    string_single, // '...'

    // Identifiers and keywords
    identifier,

    // Svelte blocks
    if_keyword, // #if
    else_keyword, // :else
    each_keyword, // #each
    await_keyword, // #await
    key_keyword, // #key
    snippet_keyword, // #snippet
    render_keyword, // @render
    html_keyword, // @html
    const_keyword, // @const
    debug_keyword, // @debug

    // Special tags
    script_start, // <script>
    script_end, // </script>
    style_start, // <style>
    style_end, // </style>

    // Content blocks (raw content between tags)
    script_content,
    style_content,

    // Comments
    comment, // <!-- ... -->
};

pub const Token = struct {
    kind: TokenKind,
    start: u32,
    end: u32,

    pub fn slice(self: Token, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
};

pub const Lexer = struct {
    source: []const u8,
    pos: u32,
    file_path: []const u8,

    // State tracking
    in_script: bool,
    in_style: bool,

    pub fn init(source: []const u8, file_path: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .file_path = file_path,
            .in_script = false,
            .in_style = false,
        };
    }

    pub fn next(self: *Lexer) Token {
        if (self.in_script) return self.lexScriptContent();
        if (self.in_style) return self.lexStyleContent();

        self.skipWhitespace();

        if (self.pos >= self.source.len) {
            return .{ .kind = .eof, .start = self.pos, .end = self.pos };
        }

        const start = self.pos;

        // Check for HTML comment
        if (self.pos + 3 < self.source.len and
            std.mem.eql(u8, self.source[self.pos .. self.pos + 4], "<!--"))
        {
            return self.lexComment(start);
        }

        const c = self.current();

        return switch (c) {
            '<' => self.lexTagStart(start),
            '>' => self.single(.gt, start),
            '{' => self.single(.lbrace, start),
            '}' => self.single(.rbrace, start),
            '=' => self.single(.eq, start),
            ':' => self.single(.colon, start),
            '|' => self.single(.pipe, start),
            '#' => self.single(.hash, start),
            '@' => self.single(.at, start),
            '/' => self.lexSlash(start),
            '"' => self.lexString('"', start),
            '\'' => self.lexString('\'', start),
            else => if (isIdentStart(c)) self.lexIdentifier(start) else self.lexText(start),
        };
    }

    fn lexTagStart(self: *Lexer, start: u32) Token {
        self.advance(); // consume '<'

        if (self.current() == '/') {
            self.advance();
            const name_start = self.pos;
            while (self.pos < self.source.len and isIdentChar(self.current())) {
                self.advance();
            }
            const name = self.source[name_start..self.pos];

            if (std.mem.eql(u8, name, "script")) {
                self.in_script = false;
                return .{ .kind = .script_end, .start = start, .end = self.pos };
            } else if (std.mem.eql(u8, name, "style")) {
                self.in_style = false;
                return .{ .kind = .style_end, .start = start, .end = self.pos };
            }
            // Backtrack to just after </ so the tag name can be lexed separately
            self.pos = name_start;
            return .{ .kind = .lt_slash, .start = start, .end = name_start };
        }

        // Check for script/style start
        const name_start = self.pos;
        while (self.pos < self.source.len and isIdentChar(self.current())) {
            self.advance();
        }
        const name = self.source[name_start..self.pos];

        if (std.mem.eql(u8, name, "script")) {
            // Skip to end of opening tag
            while (self.pos < self.source.len and self.current() != '>') {
                self.advance();
            }
            if (self.current() == '>') self.advance();
            self.in_script = true;
            return .{ .kind = .script_start, .start = start, .end = self.pos };
        } else if (std.mem.eql(u8, name, "style")) {
            while (self.pos < self.source.len and self.current() != '>') {
                self.advance();
            }
            if (self.current() == '>') self.advance();
            self.in_style = true;
            return .{ .kind = .style_start, .start = start, .end = self.pos };
        }

        // Backtrack - just return <
        self.pos = start + 1;
        return .{ .kind = .lt, .start = start, .end = start + 1 };
    }

    fn lexScriptContent(self: *Lexer) Token {
        const start = self.pos;
        while (self.pos < self.source.len) {
            if (self.pos + 8 < self.source.len and
                std.mem.eql(u8, self.source[self.pos .. self.pos + 9], "</script>"))
            {
                break;
            }
            self.advance();
        }
        if (self.pos > start) {
            return .{ .kind = .script_content, .start = start, .end = self.pos };
        }
        self.in_script = false;
        return self.next();
    }

    fn lexStyleContent(self: *Lexer) Token {
        const start = self.pos;
        while (self.pos < self.source.len) {
            if (self.pos + 7 < self.source.len and
                std.mem.eql(u8, self.source[self.pos .. self.pos + 8], "</style>"))
            {
                break;
            }
            self.advance();
        }
        if (self.pos > start) {
            return .{ .kind = .style_content, .start = start, .end = self.pos };
        }
        self.in_style = false;
        return self.next();
    }

    fn lexSlash(self: *Lexer, start: u32) Token {
        self.advance();
        if (self.current() == '>') {
            self.advance();
            return .{ .kind = .slash_gt, .start = start, .end = self.pos };
        }
        return .{ .kind = .slash, .start = start, .end = self.pos };
    }

    fn lexString(self: *Lexer, quote: u8, start: u32) Token {
        self.advance(); // consume opening quote
        while (self.pos < self.source.len) {
            const c = self.current();
            if (c == quote) {
                self.advance();
                break;
            }
            // Stop at < to avoid swallowing HTML tags (handles apostrophes in text like "ad's")
            if (c == '<') {
                break;
            }
            if (c == '\\' and self.pos + 1 < self.source.len) {
                self.advance(); // skip escape
            }
            self.advance();
        }
        return .{
            .kind = if (quote == '"') .string_double else .string_single,
            .start = start,
            .end = self.pos,
        };
    }

    fn lexIdentifier(self: *Lexer, start: u32) Token {
        while (self.pos < self.source.len and isIdentChar(self.current())) {
            self.advance();
        }
        // Handle dotted component names (Select.Root, Tooltip.Content, etc.)
        // Continue consuming .identifier patterns as part of the same token
        while (self.pos < self.source.len and self.current() == '.') {
            // Check if followed by an identifier (component continuation)
            if (self.pos + 1 < self.source.len and isIdentStart(self.source[self.pos + 1])) {
                self.advance(); // consume '.'
                while (self.pos < self.source.len and isIdentChar(self.current())) {
                    self.advance();
                }
            } else {
                break;
            }
        }
        return .{ .kind = .identifier, .start = start, .end = self.pos };
    }

    fn lexText(self: *Lexer, start: u32) Token {
        while (self.pos < self.source.len) {
            const c = self.current();
            if (c == '<' or c == '{' or c == '}') break;
            self.advance();
        }
        return .{ .kind = .text, .start = start, .end = self.pos };
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.pos < self.source.len) {
            const c = self.current();
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn lexComment(self: *Lexer, start: u32) Token {
        self.pos += 4; // skip <!--
        while (self.pos + 2 < self.source.len) {
            if (std.mem.eql(u8, self.source[self.pos .. self.pos + 3], "-->")) {
                self.pos += 3;
                break;
            }
            self.advance();
        }
        return .{ .kind = .comment, .start = start, .end = self.pos };
    }

    fn single(self: *Lexer, kind: TokenKind, start: u32) Token {
        self.advance();
        return .{ .kind = kind, .start = start, .end = self.pos };
    }

    fn current(self: *const Lexer) u8 {
        if (self.pos >= self.source.len) return 0;
        return self.source[self.pos];
    }

    fn advance(self: *Lexer) void {
        if (self.pos < self.source.len) self.pos += 1;
    }
};

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9') or c == '-';
}

test "lex simple svelte" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "<div>hello</div>";
    var lexer = Lexer.init(source, "test.svelte");

    var tokens: std.ArrayList(Token) = .empty;

    while (true) {
        const tok = lexer.next();
        try tokens.append(allocator, tok);
        if (tok.kind == .eof) break;
    }

    try std.testing.expect(tokens.items.len > 0);
}

test "lex script block" {
    const source = "<script>const x = 1;</script>";
    var lexer = Lexer.init(source, "test.svelte");

    const t1 = lexer.next();
    try std.testing.expectEqual(TokenKind.script_start, t1.kind);

    const t2 = lexer.next();
    try std.testing.expectEqual(TokenKind.script_content, t2.kind);
    try std.testing.expectEqualStrings("const x = 1;", t2.slice(source));
}
