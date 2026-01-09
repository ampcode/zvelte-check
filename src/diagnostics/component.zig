//! Component diagnostics for Svelte
//!
//! Checks for component-level issues:
//! - unused-export-let: Exported props not used in the template
//! - invalid-rune-usage: Rune calls ($state, $derived, etc.) in template expressions

const std = @import("std");
const Ast = @import("../svelte_parser.zig").Ast;
const Node = @import("../svelte_parser.zig").Node;
const NodeKind = @import("../svelte_parser.zig").NodeKind;
const ScriptData = @import("../svelte_parser.zig").ScriptData;
const Diagnostic = @import("../diagnostic.zig").Diagnostic;
const Severity = @import("../diagnostic.zig").Severity;
const Source = @import("../diagnostic.zig").Source;

/// Svelte 5 runes that must only be used in script blocks
const RUNES = [_][]const u8{
    "$state",
    "$state.raw",
    "$state.snapshot",
    "$derived",
    "$derived.by",
    "$effect",
    "$effect.pre",
    "$effect.tracking",
    "$effect.root",
    "$props",
    "$bindable",
    "$inspect",
    "$inspect.trace",
    "$host",
};

const ExportLet = struct {
    name: []const u8,
    offset: u32, // Position in source for error reporting
};

pub fn runDiagnostics(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    try checkInvalidRuneUsage(allocator, ast, diagnostics);
    try checkUnusedExportLet(allocator, ast, diagnostics);
}

/// Detects rune calls ($state, $derived, etc.) in template expressions.
/// Runes must be used in script blocks, not directly in the template.
fn checkInvalidRuneUsage(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    for (ast.nodes.items) |node| {
        const expr_content = switch (node.kind) {
            // Template expressions like {$state(0)}
            .expression,
            // Control flow block conditions: {#if $state(true)}, {#each $state(items)}
            .if_block,
            .each_block,
            .await_block,
            .key_block,
            => ast.source[node.start..node.end],
            else => continue,
        };

        for (RUNES) |rune| {
            if (containsRuneCall(expr_content, rune)) |rune_pos| {
                const loc = computeLineCol(ast.source, node.start + rune_pos);
                try diagnostics.append(allocator, .{
                    .source = .svelte,
                    .severity = .@"error",
                    .code = "invalid-rune-usage",
                    .message = try std.fmt.allocPrint(
                        allocator,
                        "{s}() can only be used inside a $derived, $effect, or at the top level of a component",
                        .{rune},
                    ),
                    .file_path = ast.file_path,
                    .start_line = loc.line,
                    .start_col = loc.col,
                    .end_line = loc.line,
                    .end_col = loc.col + @as(u32, @intCast(rune.len)),
                });
            }
        }
    }

    // Check attribute expressions for rune usage
    for (ast.attributes.items) |attr| {
        const value = attr.value orelse continue;
        if (!std.mem.startsWith(u8, value, "{")) continue;

        for (RUNES) |rune| {
            if (containsRuneCall(value, rune)) |rune_pos| {
                const loc = computeLineCol(ast.source, attr.start + rune_pos);
                try diagnostics.append(allocator, .{
                    .source = .svelte,
                    .severity = .@"error",
                    .code = "invalid-rune-usage",
                    .message = try std.fmt.allocPrint(
                        allocator,
                        "{s}() can only be used inside a $derived, $effect, or at the top level of a component",
                        .{rune},
                    ),
                    .file_path = ast.file_path,
                    .start_line = loc.line,
                    .start_col = loc.col,
                    .end_line = loc.line,
                    .end_col = loc.col + @as(u32, @intCast(rune.len)),
                });
            }
        }
    }
}

/// Checks if text contains a rune call (e.g., "$state(") with proper boundaries.
/// Returns the position of the rune if found, null otherwise.
fn containsRuneCall(text: []const u8, rune: []const u8) ?u32 {
    var search_start: usize = 0;

    while (std.mem.indexOfPos(u8, text, search_start, rune)) |pos| {
        // Check right boundary: must be followed by (
        const after_rune = pos + rune.len;
        if (after_rune >= text.len or text[after_rune] != '(') {
            search_start = pos + 1;
            continue;
        }

        // Check left boundary: must not be part of larger identifier
        if (pos > 0) {
            const before = text[pos - 1];
            if (isIdentChar(before) and before != '$') {
                search_start = pos + 1;
                continue;
            }
        }

        return @intCast(pos);
    }

    return null;
}

fn checkUnusedExportLet(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Find instance script (not context="module")
    var instance_script: ?ScriptData = null;
    for (ast.scripts.items) |script| {
        if (script.context == null) {
            instance_script = script;
            break;
        } else if (!std.mem.eql(u8, script.context.?, "module")) {
            instance_script = script;
            break;
        }
    }

    const script = instance_script orelse return;
    const script_content = ast.source[script.content_start..script.content_end];

    // Extract export let declarations
    var exports: std.ArrayList(ExportLet) = .empty;
    defer exports.deinit(allocator);
    try extractExportLets(allocator, script_content, script.content_start, &exports);

    if (exports.items.len == 0) return;

    // Collect all identifiers referenced in template
    var referenced: std.StringHashMapUnmanaged(void) = .empty;
    defer referenced.deinit(allocator);
    try collectTemplateReferences(allocator, ast, &referenced);

    // Also collect references from script (prop might be used in reactive statement)
    try collectScriptReferences(allocator, script_content, &referenced);

    // Check each export against references
    for (exports.items) |exp| {
        if (!referenced.contains(exp.name)) {
            const loc = computeLineCol(ast.source, exp.offset);
            try diagnostics.append(allocator, .{
                .source = .svelte,
                .severity = .warning,
                .code = "unused-export-let",
                .message = try std.fmt.allocPrint(
                    allocator,
                    "'{s}' is exported but never used in the component",
                    .{exp.name},
                ),
                .file_path = ast.file_path,
                .start_line = loc.line,
                .start_col = loc.col,
                .end_line = loc.line,
                .end_col = loc.col + @as(u32, @intCast(exp.name.len)),
            });
        }
    }
}

fn extractExportLets(
    allocator: std.mem.Allocator,
    content: []const u8,
    base_offset: u32,
    exports: *std.ArrayList(ExportLet),
) !void {
    var i: usize = 0;
    var brace_depth: u32 = 0;
    var paren_depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        // Track nesting depth
        if (c == '{') brace_depth += 1;
        if (c == '}' and brace_depth > 0) brace_depth -= 1;
        if (c == '(') paren_depth += 1;
        if (c == ')' and paren_depth > 0) paren_depth -= 1;

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(content, i);
            continue;
        }

        // Skip single-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            continue;
        }

        // Skip multi-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '*') {
            i += 2;
            while (i + 1 < content.len) {
                if (content[i] == '*' and content[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Only look for export let at top level
        if (brace_depth == 0 and paren_depth == 0) {
            if (startsWithKeyword(content[i..], "export")) {
                i += 6;
                i = skipWhitespace(content, i);

                if (startsWithKeyword(content[i..], "let")) {
                    i += 3;
                    i = skipWhitespace(content, i);

                    // Parse identifier
                    const name_start = i;
                    while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
                    const name = content[name_start..i];

                    if (name.len > 0) {
                        try exports.append(allocator, .{
                            .name = name,
                            .offset = base_offset + @as(u32, @intCast(name_start)),
                        });
                    }

                    // Skip to end of statement
                    while (i < content.len and content[i] != ';' and content[i] != '\n') : (i += 1) {}
                    continue;
                }
            }
        }

        i += 1;
    }
}

fn collectTemplateReferences(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    referenced: *std.StringHashMapUnmanaged(void),
) !void {
    for (ast.nodes.items) |node| {
        switch (node.kind) {
            // Template expressions {expr}
            .expression, .if_block, .each_block, .await_block, .key_block, .render, .html, .const_tag, .debug_tag => {
                const expr = ast.source[node.start..node.end];
                try extractIdentifiers(allocator, expr, referenced);
            },
            else => {},
        }
    }

    // Check attribute values for expressions
    for (ast.attributes.items) |attr| {
        if (attr.value) |value| {
            // Check for expression syntax {expr} or event handlers
            if (std.mem.startsWith(u8, value, "{") or
                std.mem.startsWith(u8, attr.name, "on:") or
                std.mem.startsWith(u8, attr.name, "bind:") or
                std.mem.startsWith(u8, attr.name, "class:") or
                std.mem.startsWith(u8, attr.name, "style:") or
                std.mem.startsWith(u8, attr.name, "use:"))
            {
                try extractIdentifiers(allocator, value, referenced);
            }
        }

        // Shorthand: {variable} as attribute
        if (attr.name.len > 0 and attr.value == null) {
            // Could be a shorthand like onclick={handleClick} where attr.name is full
            try extractIdentifiers(allocator, attr.name, referenced);
        }
    }
}

fn collectScriptReferences(
    allocator: std.mem.Allocator,
    content: []const u8,
    referenced: *std.StringHashMapUnmanaged(void),
) !void {
    // Look for reactive statements: $: foo = bar + baz
    // The identifiers on the RHS are references, but we need to skip LHS
    var i: usize = 0;

    while (i < content.len) {
        // Skip strings
        if (content[i] == '"' or content[i] == '\'' or content[i] == '`') {
            i = skipString(content, i);
            continue;
        }

        // Skip comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            continue;
        }
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '*') {
            i += 2;
            while (i + 1 < content.len) {
                if (content[i] == '*' and content[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Look for $: reactive statement
        if (content[i] == '$' and i + 1 < content.len and content[i + 1] == ':') {
            i += 2;
            // Skip to = (assignment) and then extract RHS identifiers
            while (i < content.len and content[i] != '=' and content[i] != ';' and content[i] != '\n') : (i += 1) {}
            if (i < content.len and content[i] == '=') {
                i += 1;
                // Now extract identifiers until ; or newline
                const start = i;
                while (i < content.len and content[i] != ';' and content[i] != '\n') : (i += 1) {}
                try extractIdentifiers(allocator, content[start..i], referenced);
            }
            continue;
        }

        i += 1;
    }
}

fn extractIdentifiers(
    allocator: std.mem.Allocator,
    text: []const u8,
    referenced: *std.StringHashMapUnmanaged(void),
) !void {
    var i: usize = 0;

    while (i < text.len) {
        // Skip strings
        if (text[i] == '"' or text[i] == '\'' or text[i] == '`') {
            i = skipString(text, i);
            continue;
        }

        // Look for identifier start
        if (isIdentStart(text[i])) {
            const start = i;
            while (i < text.len and isIdentChar(text[i])) : (i += 1) {}
            const ident = text[start..i];

            // Skip JavaScript keywords
            if (!isKeyword(ident)) {
                try referenced.put(allocator, ident, {});
            }
            continue;
        }

        i += 1;
    }
}

fn isKeyword(s: []const u8) bool {
    const keywords = [_][]const u8{
        "if",       "else",  "for",      "while",     "do",      "switch",
        "case",     "break", "continue", "return",    "try",     "catch",
        "finally",  "throw", "new",      "delete",    "typeof",  "instanceof",
        "void",     "this",  "super",    "class",     "extends", "import",
        "export",   "from",  "default",  "as",        "async",   "await",
        "function", "const", "let",      "var",       "in",      "of",
        "true",     "false", "null",     "undefined",
        // Svelte specific
        "each",    "await",
        "then",     "catch", "key",      "html",      "snippet", "render",
        "debug",    "const",
    };

    for (keywords) |kw| {
        if (std.mem.eql(u8, s, kw)) return true;
    }
    return false;
}

fn skipString(content: []const u8, start: usize) usize {
    if (start >= content.len) return start;
    const quote = content[start];
    var i = start + 1;
    while (i < content.len) {
        if (content[i] == '\\' and i + 1 < content.len) {
            i += 2;
            continue;
        }
        if (content[i] == quote) {
            return i + 1;
        }
        i += 1;
    }
    return i;
}

fn skipWhitespace(content: []const u8, start: usize) usize {
    var i = start;
    while (i < content.len and std.ascii.isWhitespace(content[i])) : (i += 1) {}
    return i;
}

fn startsWithKeyword(text: []const u8, keyword: []const u8) bool {
    if (text.len < keyword.len) return false;
    if (!std.mem.startsWith(u8, text, keyword)) return false;
    if (text.len > keyword.len and isIdentChar(text[keyword.len])) return false;
    return true;
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
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

test "unused-export-let: simple unused prop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  export let unusedProp;
        \\  export let usedProp;
        \\</script>
        \\<p>{usedProp}</p>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("unused-export-let", diagnostics.items[0].code.?);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.items[0].message, "unusedProp") != null);
}

test "unused-export-let: prop used in attribute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  export let className;
        \\</script>
        \\<div class={className}>Hello</div>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // className is used, so no warnings
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "unused-export-let: prop used in event handler" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  export let onClick;
        \\</script>
        \\<button on:click={onClick}>Click</button>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "unused-export-let: prop used in reactive statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  export let value;
        \\  $: doubled = value * 2;
        \\</script>
        \\<p>{doubled}</p>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // value is used in reactive statement, so no warning
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "unused-export-let: no exports" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  let internal = 5;
        \\</script>
        \\<p>{internal}</p>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "unused-export-let: prop in if block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  export let show;
        \\</script>
        \\{#if show}
        \\  <p>Visible</p>
        \\{/if}
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "extract identifiers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var refs: std.StringHashMapUnmanaged(void) = .empty;

    try extractIdentifiers(allocator, "foo + bar * 2", &refs);

    try std.testing.expect(refs.contains("foo"));
    try std.testing.expect(refs.contains("bar"));
    try std.testing.expect(!refs.contains("2")); // not an identifier
}

// ============================================================================
// invalid-rune-usage tests
// ============================================================================

test "containsRuneCall: detects rune calls" {
    try std.testing.expectEqual(@as(?u32, 0), containsRuneCall("$state(0)", "$state"));
    try std.testing.expectEqual(@as(?u32, 6), containsRuneCall("foo + $state(0)", "$state"));
    try std.testing.expectEqual(@as(?u32, 0), containsRuneCall("$derived(count * 2)", "$derived"));
    try std.testing.expectEqual(@as(?u32, 0), containsRuneCall("$effect.pre(() => {})", "$effect.pre"));
}

test "containsRuneCall: rejects non-rune patterns" {
    // Not followed by (
    try std.testing.expectEqual(@as(?u32, null), containsRuneCall("$stateValue", "$state"));
    try std.testing.expectEqual(@as(?u32, null), containsRuneCall("$state", "$state"));

    // Part of larger identifier (my$state)
    try std.testing.expectEqual(@as(?u32, null), containsRuneCall("my$state(0)", "$state"));

    // $myStore is not a rune
    try std.testing.expectEqual(@as(?u32, null), containsRuneCall("$myStore", "$state"));
}

test "invalid-rune-usage: $state in template expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<p>Count: {$state(0)}</p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("invalid-rune-usage", diagnostics.items[0].code.?);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.items[0].message, "$state") != null);
}

test "invalid-rune-usage: $derived in template" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<span>{$derived(count * 2)}</span>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("invalid-rune-usage", diagnostics.items[0].code.?);
}

test "invalid-rune-usage: rune in if block condition" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "{#if $state(true)}<p>Yes</p>{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("invalid-rune-usage", diagnostics.items[0].code.?);
}

test "invalid-rune-usage: rune in each block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "{#each $state([1, 2, 3]) as item}<li>{item}</li>{/each}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
}

test "invalid-rune-usage: rune in attribute expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<div class={$state('active')}>Styled</div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
}

test "invalid-rune-usage: regular store subscription is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<p>Store: {$myStore}</p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // $myStore is a store subscription, not a rune call - should be fine
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "invalid-rune-usage: variable referencing state is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  let count = $state(0);
        \\</script>
        \\<p>{count}</p>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Using the variable 'count' (not $state) in template is correct
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}
