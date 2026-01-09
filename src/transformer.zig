//! Svelte → TypeScript transformer
//!
//! Generates virtual .svelte.ts files that tsgo can type-check.
//! Also builds source maps for error position mapping.
//!
//! Transformation strategy:
//! 1. Detect SvelteKit route files (+page.svelte, +layout.svelte, +error.svelte)
//! 2. Add Svelte type imports (including Snippet for Svelte 5)
//! 3. Add SvelteKit route type imports (PageData, LayoutData from ./$types)
//! 4. Add Svelte 5 rune type declarations ($state, $derived, $effect, $props, etc.)
//! 5. Emit module script content (context="module")
//! 6. Emit instance script content
//! 7. Extract props from either:
//!    - `export let` declarations (Svelte 4 style)
//!    - `$props()` destructuring (Svelte 5 style)
//!    - Interface types referenced in $props<T>()
//! 8. Extract <slot> elements and snippet props to generate $$Slots interface
//! 9. Generate component class extending SvelteComponentTyped

const std = @import("std");
const Ast = @import("svelte_parser.zig").Ast;
const ScriptData = @import("svelte_parser.zig").ScriptData;
const ElementData = @import("svelte_parser.zig").ElementData;
const SourceMap = @import("source_map.zig").SourceMap;
const sveltekit = @import("sveltekit.zig");

pub const VirtualFile = struct {
    original_path: []const u8,
    virtual_path: []const u8,
    content: []const u8,
    source_map: SourceMap,
};

const PropInfo = struct {
    name: []const u8,
    type_repr: ?[]const u8,
    has_initializer: bool,
    is_bindable: bool,
};

const SlotInfo = struct {
    name: []const u8,
    props: std.ArrayList([]const u8),
};

const SnippetInfo = struct {
    name: []const u8,
    params: ?[]const u8,
};

pub fn transform(allocator: std.mem.Allocator, ast: Ast) !VirtualFile {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var mappings: std.ArrayList(SourceMap.Mapping) = .empty;
    defer mappings.deinit(allocator);

    // Detect SvelteKit route files
    const route_info = sveltekit.detectRoute(ast.file_path);

    // Header comment
    try output.appendSlice(allocator, "// Generated from ");
    try output.appendSlice(allocator, ast.file_path);
    try output.appendSlice(allocator, "\n\n");

    // Svelte type imports (including Snippet for Svelte 5)
    // SvelteComponentTyped must be a regular import (not type-only) since we extend it
    try output.appendSlice(allocator, "import { SvelteComponentTyped } from \"svelte\";\n");
    try output.appendSlice(allocator, "import type { Snippet } from \"svelte\";\n");

    // SvelteKit route type imports
    if (route_info.isRoute()) {
        try emitRouteTypeImports(allocator, &output, route_info);
    }
    try output.appendSlice(allocator, "\n");

    // Svelte 5 rune type declarations
    try output.appendSlice(allocator,
        \\// Svelte 5 rune type stubs
        \\declare function $state<T>(initial: T): T;
        \\declare namespace $state {
        \\  function raw<T>(initial: T): T;
        \\  function snapshot<T>(state: T): T;
        \\}
        \\declare function $derived<T>(expr: T): T;
        \\declare namespace $derived {
        \\  function by<T>(fn: () => T): T;
        \\}
        \\declare function $effect(fn: () => void | (() => void)): void;
        \\declare namespace $effect {
        \\  function pre(fn: () => void | (() => void)): void;
        \\  function tracking(): boolean;
        \\  function root(fn: () => void | (() => void)): () => void;
        \\}
        \\declare function $props<T = $$Props>(): T;
        \\declare function $bindable<T>(initial?: T): T;
        \\declare function $inspect<T>(...values: T[]): { with: (fn: (type: 'init' | 'update', ...values: T[]) => void) => void };
        \\declare function $host<T extends HTMLElement>(): T;
        \\
        \\// Svelte store auto-subscription stub
        \\// $storeName syntax in Svelte auto-subscribes to the store
        \\declare function __svelte_store_get<T>(store: { subscribe: (fn: (value: T) => void) => any }): T;
        \\
        \\
    );

    // Separate module and instance scripts
    var module_script: ?ScriptData = null;
    var instance_script: ?ScriptData = null;

    for (ast.scripts.items) |script| {
        if (script.context) |ctx| {
            if (std.mem.eql(u8, ctx, "module")) {
                module_script = script;
            } else {
                instance_script = script;
            }
        } else {
            instance_script = script;
        }
    }

    // Emit module script content (if any)
    if (module_script) |script| {
        try output.appendSlice(allocator, "// <script context=\"module\">\n");
        const raw_content = ast.source[script.content_start..script.content_end];
        const filtered = try filterSvelteImports(allocator, raw_content);
        const reactive_transformed = try transformReactiveStatements(allocator, filtered);
        const content = try transformStoreSubscriptions(allocator, reactive_transformed);

        try mappings.append(allocator, .{
            .svelte_offset = script.content_start,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(raw_content.len),
        });

        try output.appendSlice(allocator, content);
        try output.appendSlice(allocator, "\n\n");
    }

    // Emit instance script content (if any)
    if (instance_script) |script| {
        try output.appendSlice(allocator, "// <script>\n");
        const raw_content = ast.source[script.content_start..script.content_end];
        const filtered = try filterSvelteImports(allocator, raw_content);
        const reactive_transformed = try transformReactiveStatements(allocator, filtered);
        const content = try transformStoreSubscriptions(allocator, reactive_transformed);

        try mappings.append(allocator, .{
            .svelte_offset = script.content_start,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(raw_content.len),
        });

        try output.appendSlice(allocator, content);
        try output.appendSlice(allocator, "\n\n");
    }

    // Extract and emit template expressions for type checking
    try emitTemplateExpressions(allocator, &ast, &output, &mappings);

    // Extract props from instance script
    var props: std.ArrayList(PropInfo) = .empty;
    defer props.deinit(allocator);

    // Track if we found a Props interface to use directly
    var props_interface_name: ?[]const u8 = null;

    if (instance_script) |script| {
        const content = ast.source[script.content_start..script.content_end];

        // Try Svelte 5 $props() first
        if (try extractPropsRune(allocator, content, &props)) |interface_name| {
            props_interface_name = interface_name;
        } else {
            // Fall back to Svelte 4 export let
            try extractExportLets(allocator, content, &props);
        }
    }

    // Extract slots from AST
    var slots: std.ArrayList(SlotInfo) = .empty;
    defer {
        for (slots.items) |*slot| {
            slot.props.deinit(allocator);
        }
        slots.deinit(allocator);
    }
    try extractSlots(allocator, &ast, &slots);

    // Generate $$Props interface
    try output.appendSlice(allocator, "// Component typing\n");
    if (props_interface_name) |iface| {
        // Use the existing interface directly
        try output.appendSlice(allocator, "export type $$Props = ");
        try output.appendSlice(allocator, iface);
        try output.appendSlice(allocator, ";\n\n");
    } else {
        try output.appendSlice(allocator, "export interface $$Props {\n");
        for (props.items) |prop| {
            try output.appendSlice(allocator, "  ");
            try output.appendSlice(allocator, prop.name);
            if (prop.has_initializer) {
                try output.appendSlice(allocator, "?");
            }
            try output.appendSlice(allocator, ": ");
            if (prop.type_repr) |t| {
                try output.appendSlice(allocator, t);
            } else {
                try output.appendSlice(allocator, "any");
            }
            try output.appendSlice(allocator, ";\n");
        }
        try output.appendSlice(allocator, "}\n\n");
    }

    // Generate $$Slots interface
    try output.appendSlice(allocator, "export interface $$Slots {\n");
    var has_default_slot = false;
    for (slots.items) |slot| {
        if (std.mem.eql(u8, slot.name, "default")) {
            has_default_slot = true;
        }
        try output.appendSlice(allocator, "  ");
        try output.appendSlice(allocator, slot.name);
        if (!std.mem.eql(u8, slot.name, "default")) {
            try output.appendSlice(allocator, "?");
        }
        try output.appendSlice(allocator, ": {");
        for (slot.props.items, 0..) |prop, i| {
            if (i > 0) try output.appendSlice(allocator, ",");
            try output.appendSlice(allocator, " ");
            try output.appendSlice(allocator, prop);
            try output.appendSlice(allocator, "?: any");
        }
        if (slot.props.items.len > 0) {
            try output.appendSlice(allocator, " ");
        }
        try output.appendSlice(allocator, "};\n");
    }
    if (!has_default_slot) {
        try output.appendSlice(allocator, "  default: {};\n");
    }
    try output.appendSlice(allocator, "}\n\n");

    // Generate $$Events type
    try output.appendSlice(allocator, "export type $$Events = Record<string, any>;\n\n");

    // Generate default export
    try output.appendSlice(allocator,
        \\export default class Component extends SvelteComponentTyped<$$Props, $$Events, $$Slots> {}
        \\
    );

    // Build virtual path
    const virtual_path = try std.fmt.allocPrint(allocator, "{s}.ts", .{ast.file_path});

    return .{
        .original_path = ast.file_path,
        .virtual_path = virtual_path,
        .content = try output.toOwnedSlice(allocator),
        .source_map = .{
            .mappings = try mappings.toOwnedSlice(allocator),
            .svelte_source = ast.source,
        },
    };
}

fn extractExportLets(allocator: std.mem.Allocator, content: []const u8, props: *std.ArrayList(PropInfo)) !void {
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

                    if (name.len == 0) continue;

                    i = skipWhitespace(content, i);

                    // Optional type annotation
                    var type_repr: ?[]const u8 = null;
                    if (i < content.len and content[i] == ':') {
                        i += 1;
                        i = skipWhitespace(content, i);
                        const type_start = i;
                        i = skipTypeAnnotation(content, i);
                        const type_end = i;
                        type_repr = std.mem.trim(u8, content[type_start..type_end], " \t\n\r");
                    }

                    i = skipWhitespace(content, i);

                    // Check for initializer
                    const has_initializer = i < content.len and content[i] == '=';

                    try props.append(allocator, .{
                        .name = name,
                        .type_repr = type_repr,
                        .has_initializer = has_initializer,
                        .is_bindable = false,
                    });

                    // Skip to end of statement
                    while (i < content.len and content[i] != ';' and content[i] != '\n') : (i += 1) {}
                    continue;
                }
            }
        }

        i += 1;
    }
}

/// Extracts props from Svelte 5 $props() destructuring pattern.
/// Returns the interface name if $props<InterfaceName>() is used, otherwise null.
/// Patterns supported:
/// - `let { foo, bar } = $props()`
/// - `let { foo, bar }: Props = $props()`
/// - `let { foo = "default", bar }: Props = $props()`
/// - `let { foo = $bindable() } = $props()`
/// - `let props = $props<MyProps>()`
fn extractPropsRune(allocator: std.mem.Allocator, content: []const u8, props: *std.ArrayList(PropInfo)) !?[]const u8 {
    // Look for $props() call
    const props_call = std.mem.indexOf(u8, content, "$props") orelse return null;

    // Check for generic parameter: $props<InterfaceName>()
    var i = props_call + 6; // Skip "$props"
    i = skipWhitespace(content, i);

    var generic_type: ?[]const u8 = null;
    if (i < content.len and content[i] == '<') {
        i += 1;
        const generic_start = i;
        var depth: u32 = 1;
        while (i < content.len and depth > 0) {
            if (content[i] == '<') depth += 1;
            if (content[i] == '>') depth -= 1;
            if (depth > 0) i += 1;
        }
        generic_type = std.mem.trim(u8, content[generic_start..i], " \t\n\r");
        if (generic_type.?.len == 0) generic_type = null;
    }

    // If we have a generic type, return it as the interface name
    if (generic_type) |gt| {
        return gt;
    }

    // Otherwise, look for destructuring pattern before $props
    // Find the start of the let statement
    var let_pos: ?usize = null;
    var search_pos: usize = props_call;
    while (search_pos > 0) {
        search_pos -= 1;
        if (search_pos + 3 <= content.len and startsWithKeyword(content[search_pos..], "let")) {
            let_pos = search_pos;
            break;
        }
        if (content[search_pos] == '\n' or content[search_pos] == ';') break;
    }

    if (let_pos == null) return null;

    // Parse from 'let' to '$props'
    i = let_pos.? + 3;
    i = skipWhitespace(content, i);

    // Check for destructuring pattern
    if (i >= content.len or content[i] != '{') {
        // Not a destructuring pattern (e.g., `let props = $props<T>()`)
        return null;
    }

    // Parse destructured props: { foo, bar = "default", baz: renamed }
    i += 1; // Skip '{'
    while (i < content.len and content[i] != '}') {
        i = skipWhitespace(content, i);
        if (i >= content.len or content[i] == '}') break;

        // Parse prop name
        const name_start = i;
        while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
        const name = content[name_start..i];

        if (name.len == 0) {
            i += 1;
            continue;
        }

        i = skipWhitespace(content, i);

        // Check for renaming (prop: localName)
        if (i < content.len and content[i] == ':') {
            i += 1;
            i = skipWhitespace(content, i);
            // Skip the local name
            while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
            i = skipWhitespace(content, i);
        }

        // Check for default value
        var has_initializer = false;
        var is_bindable = false;
        if (i < content.len and content[i] == '=') {
            has_initializer = true;
            i += 1;
            i = skipWhitespace(content, i);

            // Check if default is $bindable()
            if (i + 9 <= content.len and std.mem.startsWith(u8, content[i..], "$bindable")) {
                is_bindable = true;
                i += 9;
                i = skipWhitespace(content, i);
                // Skip the $bindable(...) call
                if (i < content.len and content[i] == '(') {
                    var paren_depth: u32 = 1;
                    i += 1;
                    while (i < content.len and paren_depth > 0) {
                        if (content[i] == '(') paren_depth += 1;
                        if (content[i] == ')') paren_depth -= 1;
                        i += 1;
                    }
                }
            } else {
                // Skip the default value expression
                i = skipExpression(content, i);
            }
        }

        try props.append(allocator, .{
            .name = name,
            .type_repr = null, // Type comes from the interface
            .has_initializer = has_initializer,
            .is_bindable = is_bindable,
        });

        i = skipWhitespace(content, i);
        if (i < content.len and content[i] == ',') {
            i += 1;
        }
    }

    // Check for type annotation after destructuring: { ... }: Props
    if (i < content.len and content[i] == '}') {
        i += 1;
        i = skipWhitespace(content, i);
        if (i < content.len and content[i] == ':') {
            i += 1;
            i = skipWhitespace(content, i);
            const type_start = i;
            while (i < content.len and (isIdentChar(content[i]) or content[i] == '<' or content[i] == '>')) {
                if (content[i] == '<') {
                    // Skip generic params
                    var depth: u32 = 1;
                    i += 1;
                    while (i < content.len and depth > 0) {
                        if (content[i] == '<') depth += 1;
                        if (content[i] == '>') depth -= 1;
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
            const interface_name = std.mem.trim(u8, content[type_start..i], " \t\n\r");
            if (interface_name.len > 0) {
                return interface_name;
            }
        }
    }

    return null;
}

/// Skip an expression (for default values in destructuring)
fn skipExpression(content: []const u8, start: usize) usize {
    var i = start;
    var depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        // Track nesting
        if (c == '(' or c == '[' or c == '{') depth += 1;
        if ((c == ')' or c == ']' or c == '}') and depth > 0) {
            depth -= 1;
            i += 1;
            continue;
        }

        // Stop at , or } when at depth 0
        if (depth == 0 and (c == ',' or c == '}')) break;

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(content, i);
            continue;
        }

        i += 1;
    }

    return i;
}

fn extractSlots(allocator: std.mem.Allocator, ast: *const Ast, slots: *std.ArrayList(SlotInfo)) !void {
    for (ast.elements.items) |elem| {
        if (!std.mem.eql(u8, elem.tag_name, "slot")) continue;

        var slot_name: []const u8 = "default";
        var slot_props: std.ArrayList([]const u8) = .empty;

        for (ast.attributes.items[elem.attrs_start..elem.attrs_end]) |attr| {
            if (std.mem.eql(u8, attr.name, "name")) {
                if (attr.value) |v| slot_name = v;
            } else if (!std.mem.eql(u8, attr.name, "slot") and
                !std.mem.startsWith(u8, attr.name, "on:") and
                !std.mem.startsWith(u8, attr.name, "bind:"))
            {
                try slot_props.append(allocator, attr.name);
            }
        }

        // Check if slot already exists, merge props
        var found = false;
        for (slots.items) |*existing| {
            if (std.mem.eql(u8, existing.name, slot_name)) {
                for (slot_props.items) |prop| {
                    var has_prop = false;
                    for (existing.props.items) |ep| {
                        if (std.mem.eql(u8, ep, prop)) {
                            has_prop = true;
                            break;
                        }
                    }
                    if (!has_prop) {
                        try existing.props.append(allocator, prop);
                    }
                }
                slot_props.deinit(allocator);
                found = true;
                break;
            }
        }

        if (!found) {
            try slots.append(allocator, .{
                .name = slot_name,
                .props = slot_props,
            });
        }
    }
}

const ExprInfo = struct {
    expr: []const u8,
    offset: u32,
};

/// Extracts template expressions from the AST and emits them as TypeScript
/// statements for type-checking. Handles:
/// - {#if condition} → void (condition);
/// - {#each items as item} → void (items); declare let item: typeof items[number];
/// - {#await promise} → void (promise);
/// - {#key expr} → void (expr);
/// - {@render snippet(args)} → void (snippet(args));
///
/// Plain expressions like {variable} are NOT emitted separately because they
/// often reference loop bindings that would be out of scope in the generated TS.
fn emitTemplateExpressions(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
) !void {
    var has_expressions = false;

    for (ast.nodes.items) |node| {
        // Only emit controlling expressions from block statements.
        // Skip plain expressions {foo} since they may reference loop bindings
        // that are out of scope in the flat TS output.
        const expr_info = switch (node.kind) {
            .if_block => extractIfExpression(ast.source, node.start, node.end),
            .each_block => extractEachExpression(ast.source, node.start, node.end),
            .await_block => extractAwaitExpression(ast.source, node.start, node.end),
            .key_block => extractKeyExpression(ast.source, node.start, node.end),
            .render => extractRenderExpression(ast.source, node.start, node.end),
            else => null,
        };

        if (expr_info) |info| {
            if (!has_expressions) {
                try output.appendSlice(allocator, "// Template expressions\n");
                has_expressions = true;
            }

            // Add source mapping for the expression
            try mappings.append(allocator, .{
                .svelte_offset = node.start + info.offset,
                .ts_offset = @intCast(output.items.len),
                .len = @intCast(info.expr.len),
            });

            // Emit as a statement: void (expr);
            // Using void ensures any expression is valid as a statement
            try output.appendSlice(allocator, "void (");

            // Transform store subscriptions in template expressions
            const transformed = try transformStoreSubscriptions(allocator, info.expr);
            try output.appendSlice(allocator, transformed);

            try output.appendSlice(allocator, ");\n");
        }
    }

    if (has_expressions) {
        try output.appendSlice(allocator, "\n");
    }
}

/// Extracts the condition expression from {#if condition}
fn extractIfExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {#if condition}
    const prefix = "{#if ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find closing brace, respecting template literals and nested braces
    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
}

/// Extracts the iterable expression from {#each items as item}
fn extractEachExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {#each expr as ...}
    const prefix = "{#each ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find " as " keyword, respecting strings and nested structures
    const as_pos = findAsKeyword(content[expr_start..]) orelse return null;
    const expr = std.mem.trim(u8, content[expr_start .. expr_start + as_pos], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
}

/// Finds the position of " as " keyword in an each expression,
/// respecting strings, template literals, and nested structures.
fn findAsKeyword(expr: []const u8) ?usize {
    var i: usize = 0;
    var paren_depth: u32 = 0;
    var bracket_depth: u32 = 0;
    var brace_depth: u32 = 0;

    while (i < expr.len) {
        const c = expr[i];

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            const quote = c;
            i += 1;
            while (i < expr.len) {
                if (expr[i] == '\\' and i + 1 < expr.len) {
                    i += 2;
                    continue;
                }
                if (expr[i] == quote) {
                    i += 1;
                    break;
                }
                if (quote == '`' and expr[i] == '$' and i + 1 < expr.len and expr[i + 1] == '{') {
                    i += 2;
                    var depth: u32 = 1;
                    while (i < expr.len and depth > 0) {
                        if (expr[i] == '{') depth += 1;
                        if (expr[i] == '}') depth -= 1;
                        if (depth > 0) i += 1;
                    }
                    if (i < expr.len) i += 1;
                    continue;
                }
                i += 1;
            }
            continue;
        }

        // Track nesting
        if (c == '(') paren_depth += 1;
        if (c == ')' and paren_depth > 0) paren_depth -= 1;
        if (c == '[') bracket_depth += 1;
        if (c == ']' and bracket_depth > 0) bracket_depth -= 1;
        if (c == '{') brace_depth += 1;
        if (c == '}' and brace_depth > 0) brace_depth -= 1;

        // Check for " as " only at depth 0
        if (paren_depth == 0 and bracket_depth == 0 and brace_depth == 0) {
            if (i + 4 <= expr.len and std.mem.eql(u8, expr[i .. i + 4], " as ")) {
                return i;
            }
        }

        i += 1;
    }
    return null;
}

/// Extracts the promise expression from {#await promise}
fn extractAwaitExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {#await expr} or {#await expr then value}
    const prefix = "{#await ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find expression end (closing brace)
    const expr_end = findExpressionEnd(content, expr_start);
    var expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");

    // Check for " then " and extract only the promise part before it
    // Need to handle: "getData() then result" → "getData()"
    if (findThenKeyword(expr)) |then_pos| {
        expr = std.mem.trim(u8, expr[0..then_pos], " \t\n\r");
    }

    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
}

/// Finds the position of " then" keyword in an await expression,
/// respecting strings and template literals.
fn findThenKeyword(expr: []const u8) ?usize {
    var i: usize = 0;
    while (i < expr.len) {
        const c = expr[i];

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            const quote = c;
            i += 1;
            while (i < expr.len) {
                if (expr[i] == '\\' and i + 1 < expr.len) {
                    i += 2;
                    continue;
                }
                if (expr[i] == quote) {
                    i += 1;
                    break;
                }
                if (quote == '`' and expr[i] == '$' and i + 1 < expr.len and expr[i + 1] == '{') {
                    i += 2;
                    var depth: u32 = 1;
                    while (i < expr.len and depth > 0) {
                        if (expr[i] == '{') depth += 1;
                        if (expr[i] == '}') depth -= 1;
                        if (depth > 0) i += 1;
                    }
                    if (i < expr.len) i += 1;
                    continue;
                }
                i += 1;
            }
            continue;
        }

        // Check for " then" (with space before)
        if (i > 0 and i + 5 <= expr.len) {
            if (std.mem.eql(u8, expr[i .. i + 5], " then")) {
                // Check it's followed by space or end
                if (i + 5 == expr.len or std.ascii.isWhitespace(expr[i + 5])) {
                    return i;
                }
            }
        }

        i += 1;
    }
    return null;
}

/// Extracts the key expression from {#key expr}
fn extractKeyExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {#key expr}
    const prefix = "{#key ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find closing brace, respecting template literals and nested braces
    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
}

/// Finds the end of a template expression, respecting strings and nested braces.
fn findExpressionEnd(content: []const u8, start_pos: usize) usize {
    var i = start_pos;
    var brace_depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        // Handle strings
        if (c == '"' or c == '\'') {
            const quote = c;
            i += 1;
            while (i < content.len) {
                if (content[i] == '\\' and i + 1 < content.len) {
                    i += 2;
                    continue;
                }
                if (content[i] == quote) {
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Handle template literals
        if (c == '`') {
            i += 1;
            while (i < content.len) {
                if (content[i] == '\\' and i + 1 < content.len) {
                    i += 2;
                    continue;
                }
                if (content[i] == '`') {
                    i += 1;
                    break;
                }
                // Handle ${...} in template literals
                if (content[i] == '$' and i + 1 < content.len and content[i + 1] == '{') {
                    i += 2;
                    var template_depth: u32 = 1;
                    while (i < content.len and template_depth > 0) {
                        if (content[i] == '{') template_depth += 1;
                        if (content[i] == '}') template_depth -= 1;
                        if (template_depth > 0) i += 1;
                    }
                    if (i < content.len) i += 1;
                    continue;
                }
                i += 1;
            }
            continue;
        }

        // Track nested braces (for objects, function calls, etc.)
        if (c == '(' or c == '[' or c == '{') {
            brace_depth += 1;
            i += 1;
            continue;
        }
        if ((c == ')' or c == ']') and brace_depth > 0) {
            brace_depth -= 1;
            i += 1;
            continue;
        }

        // End at } only when at depth 0
        if (c == '}') {
            if (brace_depth == 0) {
                return i;
            }
            brace_depth -= 1;
        }

        i += 1;
    }

    return i;
}

/// Extracts expression from {@render snippet(args)}
fn extractRenderExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {@render expr}
    const prefix = "{@render ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find closing brace, respecting template literals and nested braces
    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
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

fn skipTypeAnnotation(content: []const u8, start: usize) usize {
    var i = start;
    var depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        if (c == '<' or c == '(' or c == '[' or c == '{') depth += 1;
        if ((c == '>' or c == ')' or c == ']' or c == '}') and depth > 0) depth -= 1;

        // Stop at = or ; or newline when at depth 0
        if (depth == 0 and (c == '=' or c == ';' or c == '\n')) {
            break;
        }

        i += 1;
    }

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

/// Filters out `import type ... from 'svelte'` statements from script content
/// to avoid duplicate identifier errors with our injected type imports.
/// Preserves regular imports (onMount, onDestroy, tick, etc.) from svelte.
/// Returns content with type import lines removed.
fn filterSvelteImports(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    var i: usize = 0;
    while (i < content.len) {
        // Find start of current line
        const line_start = i;

        // Find end of line
        var line_end = i;
        while (line_end < content.len and content[line_end] != '\n') : (line_end += 1) {}

        const line = content[line_start..line_end];
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Check if this is a TYPE import from 'svelte' or "svelte"
        // We only filter type imports to avoid duplicating Snippet, SvelteComponent, etc.
        // Regular imports (onMount, onDestroy, tick) are preserved
        const is_svelte_type_import = blk: {
            // Must be "import type" to be filtered
            if (!std.mem.startsWith(u8, trimmed, "import type")) break :blk false;

            // Look for from 'svelte' or from "svelte"
            if (std.mem.indexOf(u8, trimmed, "from 'svelte'")) |_| break :blk true;
            if (std.mem.indexOf(u8, trimmed, "from \"svelte\"")) |_| break :blk true;

            break :blk false;
        };

        if (!is_svelte_type_import) {
            try result.appendSlice(allocator, content[line_start..line_end]);
        }

        // Move past newline
        i = line_end;
        if (i < content.len and content[i] == '\n') {
            if (!is_svelte_type_import) {
                try result.append(allocator, '\n');
            }
            i += 1;
        }
    }

    return try result.toOwnedSlice(allocator);
}

/// Transforms Svelte store auto-subscriptions ($storeName) to __svelte_store_get(storeName).
/// This allows tsgo to type-check store access patterns without seeing undefined variables.
/// Handles cases like:
/// - `$page.url` → `__svelte_store_get(page).url`
/// - `$count + 1` → `__svelte_store_get(count) + 1`
/// - `if ($loading)` → `if (__svelte_store_get(loading))`
/// Does NOT transform:
/// - Runes like `$state`, `$derived`, `$effect`, `$props`, `$bindable`, `$inspect`, `$host`
/// - Template strings like `${expr}`
fn transformStoreSubscriptions(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    const runes = [_][]const u8{
        "state", "derived", "effect", "props", "bindable", "inspect", "host",
    };

    var i: usize = 0;
    while (i < content.len) {
        // Check for $ followed by identifier
        if (content[i] == '$') {
            // Check it's not inside a template literal ${...}
            // (We'd need to be at `${`, but we're looking at `$` so check next char isn't `{`)
            if (i + 1 < content.len and content[i + 1] == '{') {
                // Template literal interpolation - copy as-is
                try result.append(allocator, content[i]);
                i += 1;
                continue;
            }

            // Check if next char starts an identifier
            if (i + 1 < content.len and isIdentStartChar(content[i + 1])) {
                const name_start = i + 1;
                var name_end = name_start;
                while (name_end < content.len and isIdentChar(content[name_end])) : (name_end += 1) {}

                const name = content[name_start..name_end];

                // Check if this is a rune (skip transformation)
                const is_rune = for (runes) |rune| {
                    if (std.mem.eql(u8, name, rune)) break true;
                } else false;

                if (is_rune) {
                    // Keep $rune as-is
                    try result.appendSlice(allocator, content[i..name_end]);
                    i = name_end;
                    continue;
                }

                // Transform $storeName → __svelte_store_get(storeName)
                try result.appendSlice(allocator, "__svelte_store_get(");
                try result.appendSlice(allocator, name);
                try result.append(allocator, ')');
                i = name_end;
                continue;
            }
        }

        // Skip strings to avoid transforming $variables inside them
        if (content[i] == '"' or content[i] == '\'' or content[i] == '`') {
            const quote = content[i];
            try result.append(allocator, quote);
            i += 1;

            while (i < content.len) {
                if (content[i] == '\\' and i + 1 < content.len) {
                    // Escape sequence - copy both chars
                    try result.appendSlice(allocator, content[i .. i + 2]);
                    i += 2;
                    continue;
                }
                if (content[i] == quote) {
                    try result.append(allocator, quote);
                    i += 1;
                    break;
                }
                // For template literals, handle ${...} interpolations
                if (quote == '`' and content[i] == '$' and i + 1 < content.len and content[i + 1] == '{') {
                    try result.appendSlice(allocator, "${");
                    i += 2;
                    // Find matching }
                    var depth: u32 = 1;
                    while (i < content.len and depth > 0) {
                        if (content[i] == '{') depth += 1;
                        if (content[i] == '}') depth -= 1;
                        if (depth > 0) {
                            // Recursively check for $store inside interpolation
                            if (content[i] == '$' and i + 1 < content.len and isIdentStartChar(content[i + 1])) {
                                const inner_name_start = i + 1;
                                var inner_name_end = inner_name_start;
                                while (inner_name_end < content.len and isIdentChar(content[inner_name_end])) : (inner_name_end += 1) {}
                                const inner_name = content[inner_name_start..inner_name_end];

                                const is_inner_rune = for (runes) |rune| {
                                    if (std.mem.eql(u8, inner_name, rune)) break true;
                                } else false;

                                if (!is_inner_rune) {
                                    try result.appendSlice(allocator, "__svelte_store_get(");
                                    try result.appendSlice(allocator, inner_name);
                                    try result.append(allocator, ')');
                                    i = inner_name_end;
                                    continue;
                                }
                            }
                            try result.append(allocator, content[i]);
                            i += 1;
                        }
                    }
                    if (i < content.len) {
                        try result.append(allocator, '}');
                        i += 1;
                    }
                    continue;
                }
                try result.append(allocator, content[i]);
                i += 1;
            }
            continue;
        }

        // Skip single-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') {
                try result.append(allocator, content[i]);
                i += 1;
            }
            continue;
        }

        // Skip multi-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '*') {
            try result.appendSlice(allocator, "/*");
            i += 2;
            while (i + 1 < content.len) {
                if (content[i] == '*' and content[i + 1] == '/') {
                    try result.appendSlice(allocator, "*/");
                    i += 2;
                    break;
                }
                try result.append(allocator, content[i]);
                i += 1;
            }
            continue;
        }

        try result.append(allocator, content[i]);
        i += 1;
    }

    return try result.toOwnedSlice(allocator);
}

/// Transforms Svelte reactive statements ($: x = expr) into valid TypeScript (let x = expr).
/// Reactive statements in Svelte implicitly declare variables, but tsgo needs explicit declarations.
/// Handles:
/// - `$: doubled = value * 2` → `let doubled = value * 2`
/// - `$: doubled: number = value * 2` → `let doubled: number = value * 2`
/// Does NOT transform:
/// - Reactive blocks: `$: { ... }` - left as-is (valid labeled statement)
/// - Reactive expressions: `$: console.log(x)` - left as-is (valid labeled statement)
fn transformReactiveStatements(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    var i: usize = 0;
    while (i < content.len) {
        // Skip strings
        if (content[i] == '"' or content[i] == '\'' or content[i] == '`') {
            const start = i;
            i = skipStringForReactive(content, i);
            try result.appendSlice(allocator, content[start..i]);
            continue;
        }

        // Skip single-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            const start = i;
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            try result.appendSlice(allocator, content[start..i]);
            continue;
        }

        // Skip multi-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '*') {
            const start = i;
            i += 2;
            while (i + 1 < content.len) {
                if (content[i] == '*' and content[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            try result.appendSlice(allocator, content[start..i]);
            continue;
        }

        // Look for $: at start of statement
        if (content[i] == '$' and i + 1 < content.len and content[i + 1] == ':') {
            // Check that we're at the start of a statement (only whitespace since line start)
            const at_start = isAtStatementStart(content, i);

            if (at_start) {
                // Skip "$:" and any whitespace after it
                var j = i + 2;
                while (j < content.len and (content[j] == ' ' or content[j] == '\t')) : (j += 1) {}

                // Check if this is an assignment: identifier (optional type annotation) =
                if (j < content.len and isIdentStartChar(content[j])) {
                    const ident_start = j;
                    while (j < content.len and isIdentChar(content[j])) : (j += 1) {}

                    // Skip whitespace
                    while (j < content.len and (content[j] == ' ' or content[j] == '\t')) : (j += 1) {}

                    // Check for optional type annotation; default to end of identifier
                    var type_end = j;
                    if (j < content.len and content[j] == ':') {
                        j += 1;
                        j = skipTypeAnnotation(content, j);
                        type_end = j;
                        while (j < content.len and (content[j] == ' ' or content[j] == '\t')) : (j += 1) {}
                    }

                    // Check for assignment
                    if (j < content.len and content[j] == '=') {
                        // This is `$: x = expr` or `$: x: type = expr`
                        // Transform to `let x = expr` or `let x: type = expr`
                        try result.appendSlice(allocator, "let ");
                        try result.appendSlice(allocator, content[ident_start..type_end]);
                        i = j; // Continue from the '='
                        continue;
                    }
                }
            }
        }

        try result.append(allocator, content[i]);
        i += 1;
    }

    return try result.toOwnedSlice(allocator);
}

/// Checks if position i is at the start of a statement (only whitespace or statement
/// boundary characters precede it on the current line)
fn isAtStatementStart(content: []const u8, pos: usize) bool {
    if (pos == 0) return true;

    // Look backwards from pos to find if we're at statement start
    var j = pos;
    while (j > 0) {
        j -= 1;
        const c = content[j];
        if (c == '\n' or c == ';' or c == '{' or c == '}') {
            // Found statement boundary - we're at start
            return true;
        }
        if (c != ' ' and c != '\t') {
            // Found non-whitespace that isn't a boundary - not at start
            return false;
        }
    }
    // Reached start of content with only whitespace
    return true;
}

fn skipStringForReactive(content: []const u8, start: usize) usize {
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

fn isIdentStartChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

/// Emits type imports for SvelteKit route files
fn emitRouteTypeImports(allocator: std.mem.Allocator, output: *std.ArrayList(u8), route_info: sveltekit.RouteInfo) !void {
    // Build the list of types to import from ./$types
    var types: std.ArrayList([]const u8) = .empty;
    defer types.deinit(allocator);

    // Data type (PageData or LayoutData)
    if (route_info.dataTypeName()) |data_type| {
        try types.append(allocator, data_type);
    }

    // Load function types
    if (route_info.loadTypeName()) |load_type| {
        try types.append(allocator, load_type);
    }
    if (route_info.serverLoadTypeName()) |server_type| {
        try types.append(allocator, server_type);
    }

    // Action types for pages
    if (route_info.kind == .page) {
        try types.append(allocator, "Actions");
        try types.append(allocator, "ActionData");
    }

    if (types.items.len == 0) return;

    // Emit: import type { PageData, PageLoad, ... } from "./$types";
    try output.appendSlice(allocator, "import type { ");
    for (types.items, 0..) |type_name, i| {
        if (i > 0) try output.appendSlice(allocator, ", ");
        try output.appendSlice(allocator, type_name);
    }
    try output.appendSlice(allocator, " } from \"./$types\";\n");
}

test "transform simple script" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script>
        \\  let count = 0;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    try std.testing.expect(virtual.content.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let count = 0") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "SvelteComponentTyped") != null);
}

test "transform with export let" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  export let name: string;
        \\  export let count: number = 0;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "name: string") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "count?: number") != null);
}

test "transform module script" {
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

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "// <script context=\"module\">") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "VERSION") != null);
}

test "extract export lets" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content =
        \\  export let name: string;
        \\  export let count: number = 0;
        \\  export let items: string[] = [];
        \\  let internal = 5;
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    try extractExportLets(allocator, content, &props);

    try std.testing.expectEqual(@as(usize, 3), props.items.len);

    try std.testing.expectEqualStrings("name", props.items[0].name);
    try std.testing.expectEqualStrings("string", props.items[0].type_repr.?);
    try std.testing.expect(!props.items[0].has_initializer);

    try std.testing.expectEqualStrings("count", props.items[1].name);
    try std.testing.expectEqualStrings("number", props.items[1].type_repr.?);
    try std.testing.expect(props.items[1].has_initializer);

    try std.testing.expectEqualStrings("items", props.items[2].name);
    try std.testing.expectEqualStrings("string[]", props.items[2].type_repr.?);
    try std.testing.expect(props.items[2].has_initializer);
}

test "transform typescript component" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import Button from './Button.svelte';
        \\  
        \\  export let name: string;
        \\  export let count: number = 0;
        \\  
        \\  let badType: number = "not a number";
        \\</script>
        \\
        \\<main>
        \\  <h1>Hello {name}!</h1>
        \\  <Button />
        \\  <slot />
        \\</main>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "App.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify key parts of the generated TypeScript
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import { SvelteComponentTyped }") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet }") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Props") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "name: string") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "count?: number") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Slots") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "default: {}") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "extends SvelteComponentTyped<$$Props, $$Events, $$Slots>") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "badType: number = \"not a number\"") != null);
}

test "extract $props() with interface type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content =
        \\interface Props {
        \\    name: string;
        \\    count?: number;
        \\}
        \\let { name, count = 0 }: Props = $props();
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    const interface_name = try extractPropsRune(allocator, content, &props);

    try std.testing.expectEqualStrings("Props", interface_name.?);
    try std.testing.expectEqual(@as(usize, 2), props.items.len);
    try std.testing.expectEqualStrings("name", props.items[0].name);
    try std.testing.expect(!props.items[0].has_initializer);
    try std.testing.expectEqualStrings("count", props.items[1].name);
    try std.testing.expect(props.items[1].has_initializer);
}

test "extract $props() with generic type parameter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content =
        \\let props = $props<MyProps>();
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    const interface_name = try extractPropsRune(allocator, content, &props);

    try std.testing.expectEqualStrings("MyProps", interface_name.?);
    try std.testing.expectEqual(@as(usize, 0), props.items.len);
}

test "extract $props() with $bindable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content =
        \\let { value = $bindable(), name } = $props();
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    const interface_name = try extractPropsRune(allocator, content, &props);

    try std.testing.expect(interface_name == null);
    try std.testing.expectEqual(@as(usize, 2), props.items.len);
    try std.testing.expectEqualStrings("value", props.items[0].name);
    try std.testing.expect(props.items[0].has_initializer);
    try std.testing.expect(props.items[0].is_bindable);
    try std.testing.expectEqualStrings("name", props.items[1].name);
    try std.testing.expect(!props.items[1].is_bindable);
}

test "transform svelte 5 runes component" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import type { Snippet } from 'svelte';
        \\  
        \\  interface Props {
        \\    initialCount?: number;
        \\    children?: Snippet;
        \\  }
        \\  
        \\  let { initialCount = 0, children }: Props = $props();
        \\  
        \\  let count = $state(initialCount);
        \\  let doubled = $derived(count * 2);
        \\  
        \\  $effect(() => {
        \\    console.log(count);
        \\  });
        \\</script>
        \\
        \\<button onclick={() => count++}>{count}</button>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Counter.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify Snippet only appears once (in our import, not duplicated from user code)
    var snippet_count: usize = 0;
    var search_start: usize = 0;
    while (std.mem.indexOfPos(u8, virtual.content, search_start, "Snippet")) |pos| {
        snippet_count += 1;
        search_start = pos + 7;
    }
    // Should appear exactly twice: once in "import type { Snippet }"
    // and once in the Props interface "children?: Snippet"
    try std.testing.expectEqual(@as(usize, 2), snippet_count);

    // Should use Props interface directly
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type $$Props = Props;") != null);
    // Should have comprehensive rune stubs
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $derived") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $effect") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function pre(") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function root(") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $inspect") != null);
}

test "transform svelte 5 with generic $props" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  type Item = { id: number; name: string };
        \\  interface ListProps<T> {
        \\    items: T[];
        \\    selected?: T;
        \\  }
        \\  
        \\  let { items, selected } = $props<ListProps<Item>>();
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "List.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should use generic interface directly
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type $$Props = ListProps<Item>;") != null);
}

test "transform sveltekit +page.svelte route" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import type { PageData } from './$types';
        \\  let { data }: { data: PageData } = $props();
        \\</script>
        \\<h1>{data.title}</h1>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/+page.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should auto-import SvelteKit types
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { PageData, PageLoad, PageServerLoad, Actions, ActionData } from \"./$types\";") != null);
    // Should still have Svelte imports
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "SvelteComponentTyped") != null);
}

test "transform sveltekit +layout.svelte route" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import type { Snippet } from 'svelte';
        \\  let { children }: { children: Snippet } = $props();
        \\</script>
        \\{@render children()}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/+layout.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should auto-import layout types
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { LayoutData, LayoutLoad, LayoutServerLoad } from \"./$types\";") != null);
}

test "transform sveltekit +page@.svelte route variant" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\</script>
        \\<p>{count}</p>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/admin/+page@.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Route variant should still get page types
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { PageData") != null);
}

test "transform sveltekit +error.svelte route" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import { page } from '$app/stores';
        \\</script>
        \\<h1>{$page.status}</h1>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/+error.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Error pages don't have data types, so no $types import
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "from \"./$types\"") == null);
}

test "transform non-route component has no sveltekit imports" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  export let name: string;
        \\</script>
        \\<p>Hello {name}!</p>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/lib/Greeting.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Regular components should not have $types import
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "from \"./$types\"") == null);
}

test "transform store subscription basic" {
    const allocator = std.testing.allocator;

    const input = "const url = $page.url;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("const url = __svelte_store_get(page).url;", result);
}

test "transform store subscription preserves runes" {
    const allocator = std.testing.allocator;

    const input = "let count = $state(0); let doubled = $derived(count * 2);";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("let count = $state(0); let doubled = $derived(count * 2);", result);
}

test "transform store subscription multiple stores" {
    const allocator = std.testing.allocator;

    const input = "if ($page.url && $navigating) { console.log($myStore); }";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("if (__svelte_store_get(page).url && __svelte_store_get(navigating)) { console.log(__svelte_store_get(myStore)); }", result);
}

test "transform store subscription skips template literal interpolation marker" {
    const allocator = std.testing.allocator;

    const input = "const msg = `url: ${$page.url}`;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("const msg = `url: ${__svelte_store_get(page).url}`;", result);
}

test "transform store subscription skips strings" {
    const allocator = std.testing.allocator;

    const input = "const str = '$page is a store'; const real = $page;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("const str = '$page is a store'; const real = __svelte_store_get(page);", result);
}

test "transform store subscription mixed runes and stores" {
    const allocator = std.testing.allocator;

    const input = "let url = $state($page.url);";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("let url = $state(__svelte_store_get(page).url);", result);
}

test "transform component with store subscriptions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import { page } from '$app/stores';
        \\  
        \\  const url = $page.url;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have transformed $page to __svelte_store_get(page)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__svelte_store_get(page).url") != null);
    // Should NOT have untransformed $page
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$page.url") == null);
}

test "transform reactive statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  export let value: number = 0;
        \\  
        \\  $: doubled = value * 2;
        \\  $: tripled = value * 3;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Reactive.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have transformed $: to let declarations
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let doubled = value * 2") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let tripled = value * 3") != null);
    // Should NOT have raw $: labels anymore
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$: doubled") == null);
}

test "transform reactive statement with type annotation" {
    const allocator = std.testing.allocator;

    const input = "  $: count: number = value + 1;";
    const result = try transformReactiveStatements(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("  let count: number = value + 1;", result);
}

test "transform reactive statement preserves blocks" {
    const allocator = std.testing.allocator;

    // Reactive blocks should be preserved as-is (valid labeled statement)
    const input = "  $: { console.log(x); }";
    const result = try transformReactiveStatements(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("  $: { console.log(x); }", result);
}

test "transform reactive statement preserves function calls" {
    const allocator = std.testing.allocator;

    // Reactive function calls should be preserved as-is
    const input = "  $: console.log(x);";
    const result = try transformReactiveStatements(allocator, input);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("  $: console.log(x);", result);
}

test "transform template expressions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items = [1, 2, 3];
        \\  let condition = true;
        \\</script>
        \\{#if condition}
        \\  <p>Yes</p>
        \\{/if}
        \\{#each items as item}
        \\  <p>{item}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have template expressions section
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "// Template expressions") != null);
    // Should have the if condition check
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void (condition)") != null);
    // Should have the each iterable check
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void (items)") != null);
}
