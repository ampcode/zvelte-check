//! Component diagnostics for Svelte
//!
//! Checks for component-level issues:
//! - experimental_async: await expressions in templates or top-level script
//! - invalid-rune-usage: Rune calls ($state, $derived, etc.) in template expressions
//! - unused-export-let: Exported props not used in the template
//! - slot_element_deprecated: <slot> elements (use {@render} in Svelte 5)

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

const EXPERIMENTAL_ASYNC_ERROR = "Cannot use `await` in deriveds and template expressions, or at the top level of a component, unless the `experimental.async` compiler option is `true`";

const ExportLet = struct {
    name: []const u8,
    offset: u32, // Position in source for error reporting
};

pub fn runDiagnostics(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    try checkExperimentalAsync(allocator, ast, diagnostics);
    try checkInvalidRuneUsage(allocator, ast, diagnostics);
    try checkUnusedExportLet(allocator, ast, diagnostics);
    try checkSlotDeprecation(allocator, ast, diagnostics);
    try checkNonHoistableDeclarations(allocator, ast, diagnostics);
}

/// Detects await usage where experimental_async is required.
/// Checks for await in:
/// - Top-level script code (not inside functions)
/// - Template expressions
/// - Control flow block conditions
/// - Attribute expressions
fn checkExperimentalAsync(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Find instance script (not context="module")
    for (ast.scripts.items) |script| {
        if (script.context != null and std.mem.eql(u8, script.context.?, "module")) {
            continue;
        }

        const script_content = ast.source[script.content_start..script.content_end];

        // Check for top-level await in script
        if (findTopLevelAwait(script_content)) |await_pos| {
            const loc = computeLineCol(ast.source, script.content_start + await_pos);
            try diagnostics.append(allocator, .{
                .source = .svelte,
                .severity = .@"error",
                .code = "experimental_async",
                .message = EXPERIMENTAL_ASYNC_ERROR,
                .file_path = ast.file_path,
                .start_line = loc.line,
                .start_col = loc.col,
                .end_line = loc.line,
                .end_col = loc.col + 5, // "await" is 5 chars
            });
            return; // svelte-check stops after first await error
        }
    }

    // Check template expressions
    for (ast.nodes.items) |node| {
        const expr_content = switch (node.kind) {
            .expression, .if_block, .each_block, .key_block, .snippet, .render, .const_tag => ast.source[node.start..node.end],
            else => continue,
        };

        if (containsAwait(expr_content)) |await_pos| {
            const loc = computeLineCol(ast.source, node.start + await_pos);
            try diagnostics.append(allocator, .{
                .source = .svelte,
                .severity = .@"error",
                .code = "experimental_async",
                .message = EXPERIMENTAL_ASYNC_ERROR,
                .file_path = ast.file_path,
                .start_line = loc.line,
                .start_col = loc.col,
                .end_line = loc.line,
                .end_col = loc.col + 5,
            });
            return;
        }
    }

    // Check attribute expressions
    for (ast.attributes.items) |attr| {
        const value = attr.value orelse continue;
        if (!std.mem.startsWith(u8, value, "{")) continue;

        if (containsAwait(value)) |await_pos| {
            const loc = computeLineCol(ast.source, attr.start + await_pos);
            try diagnostics.append(allocator, .{
                .source = .svelte,
                .severity = .@"error",
                .code = "experimental_async",
                .message = EXPERIMENTAL_ASYNC_ERROR,
                .file_path = ast.file_path,
                .start_line = loc.line,
                .start_col = loc.col,
                .end_line = loc.line,
                .end_col = loc.col + 5,
            });
            return;
        }
    }
}

/// Find first top-level await in script content.
/// Tracks nesting depth to skip await inside functions.
fn findTopLevelAwait(content: []const u8) ?u32 {
    var i: usize = 0;
    var brace_depth: u32 = 0;
    var paren_depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        // Skip string literals
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(content, i);
            continue;
        }

        // Skip comments
        if (c == '/' and i + 1 < content.len) {
            if (content[i + 1] == '/') {
                // Line comment
                while (i < content.len and content[i] != '\n') : (i += 1) {}
                continue;
            }
            if (content[i + 1] == '*') {
                // Block comment
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
        }

        // Track nesting
        if (c == '{') brace_depth += 1;
        if (c == '}' and brace_depth > 0) brace_depth -= 1;
        if (c == '(') paren_depth += 1;
        if (c == ')' and paren_depth > 0) paren_depth -= 1;

        // Check for "await" at top level (not inside braces)
        if (brace_depth == 0 and startsWithKeyword(content[i..], "await")) {
            // Make sure it's not "awaited" or similar
            const after = i + 5;
            if (after >= content.len or !isIdentChar(content[after])) {
                return @intCast(i);
            }
        }

        i += 1;
    }

    return null;
}

/// Check if expression contains await keyword (not inside a string).
fn containsAwait(text: []const u8) ?u32 {
    var search_start: usize = 0;

    while (std.mem.indexOfPos(u8, text, search_start, "await")) |pos| {
        // Check right boundary: must not be part of larger identifier
        const after = pos + 5;
        if (after < text.len and isIdentChar(text[after])) {
            search_start = pos + 1;
            continue;
        }

        // Check left boundary: must not be part of larger identifier
        if (pos > 0 and isIdentChar(text[pos - 1])) {
            search_start = pos + 1;
            continue;
        }

        // Check if inside string
        if (isInsideString(text, pos)) {
            search_start = pos + 1;
            continue;
        }

        return @intCast(pos);
    }

    return null;
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
/// Excludes matches inside string literals.
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

        // Check if position is inside a string literal
        if (isInsideString(text, pos)) {
            search_start = pos + 1;
            continue;
        }

        return @intCast(pos);
    }

    return null;
}

/// Checks if a position in text is inside a string literal.
/// Scans from the beginning, tracking string state.
fn isInsideString(text: []const u8, target_pos: usize) bool {
    var i: usize = 0;
    while (i < target_pos and i < text.len) {
        const c = text[i];
        if (c == '"' or c == '\'' or c == '`') {
            const string_end = skipString(text, i);
            // If target position is within this string, it's inside
            if (target_pos > i and target_pos < string_end) {
                return true;
            }
            i = string_end;
        } else {
            i += 1;
        }
    }
    return false;
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

/// Warns when <slot> elements are used - deprecated in Svelte 5.
/// Users should use {@render ...} tags instead.
fn checkSlotDeprecation(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Track ignore codes from consecutive preceding comments
    var accumulated_ignore_codes: std.ArrayList([]const u8) = .empty;

    for (ast.nodes.items) |node| {
        if (node.kind == .comment) {
            const comment = ast.comments.items[node.data];
            const codes = ast.ignore_codes.items[comment.ignore_codes_start..comment.ignore_codes_end];
            for (codes) |code| {
                try accumulated_ignore_codes.append(allocator, code);
            }
            continue;
        }

        if (node.kind != .element) {
            accumulated_ignore_codes.clearRetainingCapacity();
            continue;
        }

        const elem = ast.elements.items[node.data];
        if (!std.mem.eql(u8, elem.tag_name, "slot")) {
            accumulated_ignore_codes.clearRetainingCapacity();
            continue;
        }

        // Check if suppressed by svelte-ignore
        var is_ignored = false;
        for (accumulated_ignore_codes.items) |ignored| {
            if (std.mem.eql(u8, ignored, "slot_element_deprecated")) {
                is_ignored = true;
                break;
            }
        }
        accumulated_ignore_codes.clearRetainingCapacity();
        if (is_ignored) continue;

        const loc = computeLineCol(ast.source, node.start);
        try diagnostics.append(allocator, .{
            .source = .svelte,
            .severity = .warning,
            .code = "slot_element_deprecated",
            .message = try allocator.dupe(u8, "Using `<slot>` to render parent content is deprecated. Use `{@render ...}` tags instead\nhttps://svelte.dev/e/slot_element_deprecated"),
            .file_path = ast.file_path,
            .start_line = loc.line,
            .start_col = loc.col,
            .end_line = loc.line,
            .end_col = loc.col + @as(u32, @intCast(elem.tag_name.len)) + 1, // +1 for <
        });
    }
}

/// Detects non-hoistable TypeScript declarations in instance scripts.
/// TypeScript `namespace` and `enum` declarations are not supported in Svelte instance
/// scripts because they cannot be hoisted out of the component's scope. These constructs
/// should be moved to a separate .ts module and imported.
///
/// Note: This only applies to TypeScript (lang="ts") instance scripts. Module scripts
/// (context="module") are at module scope where these declarations are valid.
fn checkNonHoistableDeclarations(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    diagnostics: *std.ArrayList(Diagnostic),
) !void {
    // Find instance script (not context="module") with lang="ts"
    for (ast.scripts.items) |script| {
        // Skip module scripts - they're at module scope where namespace/enum are allowed
        if (script.context != null and std.mem.eql(u8, script.context.?, "module")) {
            continue;
        }

        // Only check TypeScript instance scripts
        const is_typescript = if (script.lang) |lang|
            std.mem.eql(u8, lang, "ts") or std.mem.eql(u8, lang, "typescript")
        else
            false;

        // generics attribute also implies TypeScript mode
        const has_generics = script.generics != null;

        if (!is_typescript and !has_generics) continue;

        // For generic components, the script is wrapped in a function which makes
        // namespace/enum naturally invalid - TypeScript will report the error.
        // We only need to catch this for non-generic TypeScript scripts.
        if (has_generics) continue;

        const script_content = ast.source[script.content_start..script.content_end];

        // Scan for namespace/enum declarations at top level
        try scanNonHoistableDeclarations(allocator, script_content, script.content_start, ast.source, ast.file_path, diagnostics);
    }
}

/// Scans script content for top-level namespace/enum declarations.
fn scanNonHoistableDeclarations(
    allocator: std.mem.Allocator,
    content: []const u8,
    base_offset: u32,
    full_source: []const u8,
    file_path: []const u8,
    diagnostics: *std.ArrayList(Diagnostic),
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

        // Only look at top-level declarations
        if (brace_depth == 0 and paren_depth == 0 and isIdentStart(c)) {
            const kw_start = i;
            while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
            const ident = content[kw_start..i];

            if (std.mem.eql(u8, ident, "namespace")) {
                const abs_offset = base_offset + @as(u32, @intCast(kw_start));
                const location = computeLineCol(full_source, abs_offset);

                try diagnostics.append(allocator, .{
                    .source = .svelte,
                    .severity = .@"error",
                    .code = "non_hoistable_declaration",
                    .message = "A namespace declaration is only allowed at the top level of a namespace or module.",
                    .file_path = file_path,
                    .start_line = location.line,
                    .start_col = location.col,
                    .end_line = location.line,
                    .end_col = location.col + @as(u32, @intCast(ident.len)),
                });
            }
            continue;
        }

        i += 1;
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

// ============================================================================
// slot-element-deprecated tests
// ============================================================================

test "slot-element-deprecated: warns on <slot>" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<slot />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("slot_element_deprecated", diagnostics.items[0].code.?);
    try std.testing.expectEqual(Severity.warning, diagnostics.items[0].severity);
}

test "slot-element-deprecated: warns on <slot> with props" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<slot {x} {y} />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("slot_element_deprecated", diagnostics.items[0].code.?);
}

test "slot-element-deprecated: respects svelte-ignore" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<!-- svelte-ignore slot_element_deprecated -->\n<slot />";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "slot-element-deprecated: regular elements not affected" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<div>content</div>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

// ============================================================================
// experimental_async tests
// ============================================================================

test "experimental_async: await at top level in script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  const foo = await fetch('/api');
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("experimental_async", diagnostics.items[0].code.?);
}

test "experimental_async: await inside function is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\  async function load() {
        \\    const data = await fetch('/api');
        \\    return data;
        \\  }
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "experimental_async: await in template expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<p>{await promise}</p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("experimental_async", diagnostics.items[0].code.?);
}

test "experimental_async: await in if block condition" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "{#if await promise}<p>yes</p>{/if}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("experimental_async", diagnostics.items[0].code.?);
}

test "experimental_async: await in each block" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "{#each await items as item}<li>{item}</li>{/each}";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
}

test "experimental_async: string containing await is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source = "<p>{'await is a keyword'}</p>";
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // "await" inside a string is not an await expression
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

// ============================================================================
// non-hoistable-declaration tests
// ============================================================================

test "non-hoistable-declaration: namespace in TypeScript instance script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script lang="ts">
        \\namespace A {
        \\    export type Abc = number;
        \\}
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items.len);
    try std.testing.expectEqualStrings("non_hoistable_declaration", diagnostics.items[0].code.?);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.items[0].message, "namespace") != null);
}

test "non-hoistable-declaration: namespace in module script is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script context="module" lang="ts">
        \\namespace A {
        \\    export type Abc = number;
        \\}
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // Module scripts are at module scope where namespace is valid
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "non-hoistable-declaration: namespace in JavaScript script is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script>
        \\const namespace = { A: 1 };
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // JavaScript scripts don't have TypeScript namespace declarations
    // "namespace" here is just a variable name
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}

test "non-hoistable-declaration: namespace inside function is valid" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Parser = @import("../svelte_parser.zig").Parser;
    const source =
        \\<script lang="ts">
        \\function foo() {
        \\    const namespace = 1;
        \\}
        \\</script>
    ;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    try runDiagnostics(allocator, &ast, &diagnostics);

    // "namespace" inside a function is just a variable, not a declaration
    try std.testing.expectEqual(@as(usize, 0), diagnostics.items.len);
}
