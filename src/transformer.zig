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

const EachBindingInfo = struct {
    iterable: []const u8,
    item_binding: []const u8,
    index_binding: ?[]const u8,
    offset: u32,
};

const ConstBindingInfo = struct {
    name: []const u8,
    expr: []const u8,
    offset: u32,
};

const SnippetParamInfo = struct {
    params: []const u8,
    offset: u32,
};

/// Extracts template expressions from the AST and emits them as TypeScript
/// statements for type-checking. Handles:
/// - {#if condition} → void (condition);
/// - {#each items as item, i} → void (items); let item: typeof (items)[number]; let i: number;
/// - {#await promise} → void (promise);
/// - {#key expr} → void (expr);
/// - {@render snippet(args)} → void (snippet(args));
/// - {#snippet name(params)} → function declarations with params
/// - {@const x = expr} → let x = expr;
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
        switch (node.kind) {
            .each_block => {
                if (extractEachBindings(ast.source, node.start, node.end)) |binding| {
                    if (!has_expressions) {
                        try output.appendSlice(allocator, "// Template expressions\n");
                        has_expressions = true;
                    }

                    try mappings.append(allocator, .{
                        .svelte_offset = node.start + binding.offset,
                        .ts_offset = @intCast(output.items.len),
                        .len = @intCast(binding.iterable.len),
                    });

                    const transformed_iterable = try transformStoreSubscriptions(allocator, binding.iterable);
                    try output.appendSlice(allocator, "void (");
                    try output.appendSlice(allocator, transformed_iterable);
                    try output.appendSlice(allocator, ");\n");

                    try emitEachBindingDeclarations(allocator, output, binding, transformed_iterable);
                }
            },
            .snippet => {
                if (extractSnippetParams(ast.source, node.start, node.end)) |info| {
                    if (!has_expressions) {
                        try output.appendSlice(allocator, "// Template expressions\n");
                        has_expressions = true;
                    }
                    try emitSnippetParamDeclarations(allocator, output, info.params);
                }
            },
            .const_tag => {
                if (extractConstBinding(ast.source, node.start, node.end)) |binding| {
                    if (!has_expressions) {
                        try output.appendSlice(allocator, "// Template expressions\n");
                        has_expressions = true;
                    }

                    try mappings.append(allocator, .{
                        .svelte_offset = node.start + binding.offset,
                        .ts_offset = @intCast(output.items.len),
                        .len = @intCast(binding.name.len + binding.expr.len + 3),
                    });

                    // Emit as: var name = expr;
                    // Using var allows redeclaration for multiple blocks with same name
                    const transformed_expr = try transformStoreSubscriptions(allocator, binding.expr);
                    try output.appendSlice(allocator, "var ");
                    try output.appendSlice(allocator, binding.name);
                    try output.appendSlice(allocator, " = ");
                    try output.appendSlice(allocator, transformed_expr);
                    try output.appendSlice(allocator, ";\n");
                }
            },
            .if_block, .await_block, .key_block, .render => {
                const expr_info = switch (node.kind) {
                    .if_block => extractIfExpression(ast.source, node.start, node.end),
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

                    try mappings.append(allocator, .{
                        .svelte_offset = node.start + info.offset,
                        .ts_offset = @intCast(output.items.len),
                        .len = @intCast(info.expr.len),
                    });

                    try output.appendSlice(allocator, "void (");
                    const transformed = try transformStoreSubscriptions(allocator, info.expr);
                    try output.appendSlice(allocator, transformed);
                    try output.appendSlice(allocator, ");\n");
                }
            },
            else => {},
        }
    }

    if (has_expressions) {
        try output.appendSlice(allocator, "\n");
    }
}

/// Emits variable declarations for each block bindings
fn emitEachBindingDeclarations(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    binding: EachBindingInfo,
    iterable: []const u8,
) !void {
    // Emit item binding: var item = (iterable)[0];
    // Using var allows redeclaration for multiple blocks with same name
    try output.appendSlice(allocator, "var ");
    try output.appendSlice(allocator, binding.item_binding);
    try output.appendSlice(allocator, " = (");
    try output.appendSlice(allocator, iterable);
    try output.appendSlice(allocator, ")[0];\n");

    // Emit index binding if present: var i = 0;
    if (binding.index_binding) |idx| {
        try output.appendSlice(allocator, "var ");
        try output.appendSlice(allocator, idx);
        try output.appendSlice(allocator, " = 0;\n");
    }
}

/// Emits variable declarations for snippet parameters
fn emitSnippetParamDeclarations(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    params: []const u8,
) !void {
    var i: usize = 0;
    while (i < params.len) {
        // Skip whitespace
        while (i < params.len and std.ascii.isWhitespace(params[i])) : (i += 1) {}
        if (i >= params.len) break;

        const start = i;

        // Check for destructuring patterns (object or array)
        if (params[i] == '{' or params[i] == '[') {
            const open_char = params[i];
            const close_char: u8 = if (open_char == '{') '}' else ']';
            i += 1;
            var depth: u32 = 1;
            while (i < params.len and depth > 0) {
                if (params[i] == open_char) depth += 1;
                if (params[i] == close_char) depth -= 1;
                i += 1;
            }

            const pattern = std.mem.trim(u8, params[start..i], " \t\n\r");

            // Check for type annotation after destructuring
            while (i < params.len and std.ascii.isWhitespace(params[i])) : (i += 1) {}
            var type_annotation: ?[]const u8 = null;
            if (i < params.len and params[i] == ':') {
                i += 1;
                const type_start = i;
                var type_depth: u32 = 0;
                while (i < params.len) {
                    const c = params[i];
                    if (c == '<' or c == '(' or c == '[' or c == '{') type_depth += 1;
                    if (c == '>' or c == ')' or c == ']' or c == '}') {
                        if (type_depth > 0) {
                            type_depth -= 1;
                            i += 1;
                            continue;
                        }
                    }
                    if (type_depth == 0 and (c == ',' or c == '=')) break;
                    i += 1;
                }
                type_annotation = std.mem.trim(u8, params[type_start..i], " \t\n\r");
            }

            // Skip default value if present
            while (i < params.len and std.ascii.isWhitespace(params[i])) : (i += 1) {}
            if (i < params.len and params[i] == '=') {
                i += 1;
                var eq_depth: u32 = 0;
                while (i < params.len) {
                    const c = params[i];
                    if (c == '(' or c == '[' or c == '{') eq_depth += 1;
                    if ((c == ')' or c == ']' or c == '}') and eq_depth > 0) eq_depth -= 1;
                    if (eq_depth == 0 and c == ',') break;
                    i += 1;
                }
            }

            // Emit destructuring pattern declaration
            try output.appendSlice(allocator, "var ");
            try output.appendSlice(allocator, pattern);
            try output.appendSlice(allocator, ": ");
            if (type_annotation) |t| {
                try output.appendSlice(allocator, t);
            } else {
                try output.appendSlice(allocator, "any");
            }
            try output.appendSlice(allocator, ";\n");

            // Skip comma
            if (i < params.len and params[i] == ',') i += 1;
            continue;
        }

        // Find end of parameter name (stop at : for type, , for next, ) for end)
        while (i < params.len) {
            const c = params[i];
            if (c == ':' or c == ',' or c == '=' or c == ')') break;
            i += 1;
        }

        var param_name = std.mem.trim(u8, params[start..i], " \t\n\r");
        if (param_name.len > 0) {
            // Check if parameter is optional (ends with ?)
            var is_optional = false;
            if (param_name.len > 0 and param_name[param_name.len - 1] == '?') {
                is_optional = true;
                param_name = param_name[0 .. param_name.len - 1];
            }

            // Check if there's a type annotation
            var type_annotation: ?[]const u8 = null;
            if (i < params.len and params[i] == ':') {
                i += 1;
                const type_start = i;
                var depth: u32 = 0;
                while (i < params.len) {
                    const c = params[i];
                    if (c == '<' or c == '(' or c == '[' or c == '{') depth += 1;
                    if (c == '>' or c == ')' or c == ']' or c == '}') {
                        if (depth > 0) {
                            depth -= 1;
                            i += 1;
                            continue;
                        }
                    }
                    if (depth == 0 and (c == ',' or c == '=')) break;
                    i += 1;
                }
                type_annotation = std.mem.trim(u8, params[type_start..i], " \t\n\r");
            }

            // Skip default value if present
            if (i < params.len and params[i] == '=') {
                i += 1;
                var depth: u32 = 0;
                while (i < params.len) {
                    const c = params[i];
                    if (c == '(' or c == '[' or c == '{') depth += 1;
                    if ((c == ')' or c == ']' or c == '}') and depth > 0) depth -= 1;
                    if (depth == 0 and c == ',') break;
                    i += 1;
                }
            }

            // Use var for redeclaration safety across multiple snippets
            try output.appendSlice(allocator, "var ");
            try output.appendSlice(allocator, param_name);
            try output.appendSlice(allocator, ": ");
            if (type_annotation) |t| {
                try output.appendSlice(allocator, t);
                if (is_optional) {
                    try output.appendSlice(allocator, " | undefined");
                }
            } else if (is_optional) {
                try output.appendSlice(allocator, "any | undefined");
            } else {
                try output.appendSlice(allocator, "any");
            }
            try output.appendSlice(allocator, ";\n");
        }

        // Skip comma
        if (i < params.len and params[i] == ',') i += 1;
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

/// Extracts iterable and bindings from {#each items as item, i}
fn extractEachBindings(source: []const u8, start: u32, end: u32) ?EachBindingInfo {
    const content = source[start..end];
    // Format: {#each expr as item, index (key)}
    const prefix = "{#each ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find " as " keyword
    const as_pos = findAsKeyword(content[expr_start..]) orelse return null;
    const iterable = std.mem.trim(u8, content[expr_start .. expr_start + as_pos], " \t\n\r");
    if (iterable.len == 0) return null;

    // Parse bindings after "as"
    const binding_start = expr_start + as_pos + 4; // skip " as "
    const binding_end = findEachBindingEnd(content, binding_start);
    const bindings = std.mem.trim(u8, content[binding_start..binding_end], " \t\n\r");
    if (bindings.len == 0) return null;

    // Split on comma to get item and optional index, respecting brackets
    var item_binding: []const u8 = bindings;
    var index_binding: ?[]const u8 = null;

    // Find comma at top level (not inside brackets)
    if (findTopLevelComma(bindings)) |comma_pos| {
        item_binding = std.mem.trim(u8, bindings[0..comma_pos], " \t\n\r");
        const after_comma = std.mem.trim(u8, bindings[comma_pos + 1 ..], " \t\n\r");
        // Index might be followed by (key) - extract just the identifier
        if (std.mem.indexOf(u8, after_comma, "(")) |paren_pos| {
            index_binding = std.mem.trim(u8, after_comma[0..paren_pos], " \t\n\r");
        } else {
            index_binding = after_comma;
        }
    }

    return .{
        .iterable = iterable,
        .item_binding = item_binding,
        .index_binding = index_binding,
        .offset = @intCast(expr_start),
    };
}

/// Finds the position of a comma at the top level (not inside brackets)
fn findTopLevelComma(text: []const u8) ?usize {
    var i: usize = 0;
    var bracket_depth: u32 = 0;
    var brace_depth: u32 = 0;
    var paren_depth: u32 = 0;

    while (i < text.len) {
        const c = text[i];
        switch (c) {
            '[' => bracket_depth += 1,
            ']' => {
                if (bracket_depth > 0) bracket_depth -= 1;
            },
            '{' => brace_depth += 1,
            '}' => {
                if (brace_depth > 0) brace_depth -= 1;
            },
            '(' => paren_depth += 1,
            ')' => {
                if (paren_depth > 0) paren_depth -= 1;
            },
            ',' => {
                if (bracket_depth == 0 and brace_depth == 0 and paren_depth == 0) {
                    return i;
                }
            },
            else => {},
        }
        i += 1;
    }
    return null;
}

/// Finds the end of each block bindings (before key expression or closing brace)
/// Respects nested braces and brackets for destructuring patterns.
fn findEachBindingEnd(content: []const u8, start: usize) usize {
    var i = start;
    var paren_depth: u32 = 0;
    var brace_depth: u32 = 0;
    var bracket_depth: u32 = 0;

    while (i < content.len) {
        const c = content[i];

        switch (c) {
            '[' => bracket_depth += 1,
            ']' => {
                if (bracket_depth > 0) bracket_depth -= 1;
            },
            '{' => brace_depth += 1,
            '}' => {
                // End at } only when at depth 0
                if (brace_depth == 0 and bracket_depth == 0 and paren_depth == 0) {
                    return i;
                }
                if (brace_depth > 0) brace_depth -= 1;
            },
            '(' => {
                // Stop at top-level ( which is the key expression
                if (paren_depth == 0 and brace_depth == 0 and bracket_depth == 0) {
                    return i;
                }
                paren_depth += 1;
            },
            ')' => {
                if (paren_depth > 0) paren_depth -= 1;
            },
            else => {},
        }

        i += 1;
    }
    return i;
}

/// Extracts parameters from {#snippet name(params)}
fn extractSnippetParams(source: []const u8, start: u32, end: u32) ?SnippetParamInfo {
    const content = source[start..end];
    // Format: {#snippet name(params)}
    const prefix = "{#snippet ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const name_start = idx + prefix.len;

    // Find opening paren
    const paren_pos = std.mem.indexOf(u8, content[name_start..], "(") orelse return null;
    const params_start = name_start + paren_pos + 1;

    // Find closing paren
    var paren_depth: u32 = 1;
    var i = params_start;
    while (i < content.len and paren_depth > 0) {
        if (content[i] == '(') paren_depth += 1;
        if (content[i] == ')') paren_depth -= 1;
        if (paren_depth > 0) i += 1;
    }

    const params = std.mem.trim(u8, content[params_start..i], " \t\n\r");
    if (params.len == 0) return null;

    return .{
        .params = params,
        .offset = @intCast(params_start),
    };
}

/// Extracts binding from {@const name = expr}
fn extractConstBinding(source: []const u8, start: u32, end: u32) ?ConstBindingInfo {
    const content = source[start..end];
    // Format: {@const name = expr}
    const prefix = "{@const ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const binding_start = idx + prefix.len;

    // Find = sign
    const eq_pos = std.mem.indexOf(u8, content[binding_start..], "=") orelse return null;
    const name = std.mem.trim(u8, content[binding_start .. binding_start + eq_pos], " \t\n\r");
    if (name.len == 0) return null;

    // Find expression (until closing brace), respecting nested braces and template literals
    const expr_start = binding_start + eq_pos + 1;
    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;

    return .{
        .name = name,
        .expr = expr,
        .offset = @intCast(binding_start),
    };
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
        const line_start = i;

        var line_end = i;
        while (line_end < content.len and content[line_end] != '\n') : (line_end += 1) {}

        const line = content[line_start..line_end];
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Check import source - we filter types from 'svelte' and './$types'
        const is_svelte_import = std.mem.indexOf(u8, trimmed, "from 'svelte'") != null or
            std.mem.indexOf(u8, trimmed, "from \"svelte\"") != null;
        const is_types_import = std.mem.indexOf(u8, trimmed, "from './$types'") != null or
            std.mem.indexOf(u8, trimmed, "from \"./$types\"") != null;
        const should_filter = is_svelte_import or is_types_import;

        if (should_filter and std.mem.startsWith(u8, trimmed, "import")) {
            // Case 1: `import type { ... } from ...` - filter entire line
            if (std.mem.startsWith(u8, trimmed, "import type")) {
                // Skip this line entirely
            } else if (try filterMixedImport(allocator, trimmed)) |filtered_line| {
                // Case 2: `import { type X, y } from ...` - remove type specifiers
                try result.appendSlice(allocator, filtered_line);
            } else {
                // No types to filter, keep line as-is
                try result.appendSlice(allocator, line);
            }
        } else {
            try result.appendSlice(allocator, line);
        }

        i = line_end;
        if (i < content.len and content[i] == '\n') {
            try result.append(allocator, '\n');
            i += 1;
        }
    }

    return try result.toOwnedSlice(allocator);
}

/// Filters type specifiers from mixed imports like `import { type Snippet, tick } from 'svelte'`.
/// Returns the filtered line, or null if no changes were made.
fn filterMixedImport(allocator: std.mem.Allocator, line: []const u8) !?[]const u8 {
    // Find the import specifier list: { ... }
    const brace_start = std.mem.indexOf(u8, line, "{") orelse return null;
    const brace_end = std.mem.indexOf(u8, line, "}") orelse return null;
    if (brace_start >= brace_end) return null;

    const specifiers = line[brace_start + 1 .. brace_end];

    // Parse specifiers and filter out "type X" ones
    var filtered_specs: std.ArrayList([]const u8) = .empty;
    defer filtered_specs.deinit(allocator);

    var has_changes = false;
    var spec_iter = std.mem.splitScalar(u8, specifiers, ',');
    while (spec_iter.next()) |spec_raw| {
        const spec = std.mem.trim(u8, spec_raw, " \t\r\n");
        if (spec.len == 0) continue;

        // Check if this is a type specifier: "type X" or "type X as Y"
        if (std.mem.startsWith(u8, spec, "type ")) {
            has_changes = true;
            // Skip this specifier
        } else {
            try filtered_specs.append(allocator, spec);
        }
    }

    if (!has_changes) return null;

    // Rebuild the import line
    var new_line: std.ArrayList(u8) = .empty;
    defer new_line.deinit(allocator);

    if (filtered_specs.items.len == 0) {
        // All specifiers were types - skip the entire import
        return "";
    }

    try new_line.appendSlice(allocator, line[0 .. brace_start + 1]);
    try new_line.append(allocator, ' ');
    for (filtered_specs.items, 0..) |spec, idx| {
        if (idx > 0) try new_line.appendSlice(allocator, ", ");
        try new_line.appendSlice(allocator, spec);
    }
    try new_line.appendSlice(allocator, " ");
    try new_line.appendSlice(allocator, line[brace_end..]);

    return try new_line.toOwnedSlice(allocator);
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
    // Should have variable declaration for each binding
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var item = (items)[0];") != null);
}

test "transform each with index" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items = ['a', 'b', 'c'];
        \\</script>
        \\{#each items as item, i}
        \\  <p>{i}: {item}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have item declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var item = (items)[0];") != null);
    // Should have index declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var i = 0;") != null);
}

test "transform each with key" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items = [{id: 1, name: 'a'}];
        \\</script>
        \\{#each items as item, index (item.id)}
        \\  <p>{item.name}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have item declaration (not including key expr)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var item = (items)[0];") != null);
    // Should have index declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var index = 0;") != null);
}

test "transform snippet with params" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\</script>
        \\{#snippet greeting(name: string, age: number)}
        \\  <p>Hello {name}, you are {age}</p>
        \\{/snippet}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have param declarations with types
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name: string;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var age: number;") != null);
}

test "transform snippet with import type param" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\</script>
        \\{#snippet shortcut(key: import('svelte').Snippet | string, label: string)}
        \\  <span>{label}</span>
        \\{/snippet}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should handle import() types with parentheses correctly
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var key: import('svelte').Snippet | string;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var label: string;") != null);
}

test "transform const binding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items = [1, 2, 3];
        \\</script>
        \\{#each items as item, index}
        \\  {@const doubled = item * 2}
        \\  <p>{doubled}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have const declaration (TypeScript infers the type)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var doubled = item * 2;") != null);
}

test "debug Svelte5EdgeCases optional param" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    let count = $state(0);
        \\</script>
        \\
        \\{#snippet optionalParam(name?: string)}
        \\    <p>Hello {name ?? 'World'}</p>
        \\{/snippet}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should generate valid TypeScript with | undefined, not ?
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name: string | undefined;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name?: string;") == null);
}

test "each block with destructuring pattern" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    const entries: [string, number][] = [["a", 1]];
        \\</script>
        \\
        \\{#each entries as [k, v]}
        \\    <p>{k} = {v}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should handle destructuring correctly without breaking
    // The binding should be "[k, v]" not split incorrectly
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var [k, v] = (entries)[0];") != null);
    // Should NOT have broken syntax like "var v]"
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var v]") == null);
}

test "each block with object destructuring" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    interface Item { id: number; value: number; }
        \\    let items: Item[] = [];
        \\</script>
        \\
        \\{#each items as { id, value }}
        \\    <p>{id} = {value}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should handle object destructuring correctly
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var { id, value } = (items)[0];") != null);
}

test "snippet with object destructuring param" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    let count = $state(0);
        \\</script>
        \\
        \\{#snippet child({ wrapperProps, props, open })}
        \\    <div {...wrapperProps}>
        \\        <span>{open}</span>
        \\    </div>
        \\{/snippet}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should handle object destructuring as a single pattern
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var { wrapperProps, props, open }: any;") != null);
    // Should NOT have broken syntax
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var { wrapperProps: any;") == null);
}

test "const tag with template literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    interface Item { id: number; value: number; }
        \\    let items: Item[] = [];
        \\</script>
        \\
        \\{#each items as item}
        \\    {@const label = `Item ${item.id}`}
        \\    <p>{label}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "ConstTag.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Template literal should be complete with closing backtick
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var label = `Item ${item.id}`;") != null);
}
