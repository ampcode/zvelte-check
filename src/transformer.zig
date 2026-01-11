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
//! 9. Generate component export using Component interface (Svelte 5 style)

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

const ExportInfo = struct {
    name: []const u8,
    type_repr: ?[]const u8,
};

pub fn transform(allocator: std.mem.Allocator, ast: Ast) !VirtualFile {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    // Pre-size output buffer: source length + overhead for type stubs and component class
    try output.ensureTotalCapacity(allocator, ast.source.len + 4096);

    var mappings: std.ArrayList(SourceMap.Mapping) = .empty;
    defer mappings.deinit(allocator);
    // Pre-size mappings: typically one mapping per script block plus template expressions
    try mappings.ensureTotalCapacity(allocator, 16);

    // Detect SvelteKit route files
    const route_info = sveltekit.detectRoute(ast.file_path);

    // Header comment
    try output.appendSlice(allocator, "// Generated from ");
    try output.appendSlice(allocator, ast.file_path);
    try output.appendSlice(allocator, "\n\n");

    // Svelte type imports
    // Import Component for Svelte 5 type compatibility (makes ComponentProps work)
    // Use a private alias to avoid conflicts with user imports of Component
    try output.appendSlice(allocator, "import type { Component as __SvelteComponentType__ } from \"svelte\";\n");
    // Note: We don't auto-import Snippet - if users need it, they import it themselves.
    // Auto-importing caused "declared but never used" errors for files that don't use Snippet types.

    // SvelteKit route type imports are NOT auto-generated
    // User code imports from './$types' which resolves via SvelteKit's generated types
    // in .svelte-kit/types/ (requires SvelteKit to be built/dev'd first)
    _ = route_info;
    try output.appendSlice(allocator, "\n");

    // Svelte 5 rune type declarations
    try output.appendSlice(allocator,
        \\// Svelte 5 rune type stubs
        \\// $state with no argument returns undefined; with explicit T but no initializer returns T | undefined
        \\declare function $state(): undefined;
        \\declare function $state<T>(initial: T): T;
        \\declare function $state<T>(): T | undefined;
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
        \\// This function extracts the value type from a store's subscribe method
        \\type SvelteStore<T> = { subscribe: (run: (value: T) => any, invalidate?: any) => any };
        \\declare function __svelte_store_get<T>(store: SvelteStore<T>): T;
        \\declare function __svelte_store_get<Store extends SvelteStore<any> | undefined | null>(store: Store): Store extends SvelteStore<infer T> ? T : Store;
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

        // Emit generic type declarations if present (Svelte 5 generics="T" attribute)
        if (script.generics) |generics| {
            try emitGenericTypeDeclarations(allocator, &output, generics);
        }

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

    // Extract declared names from scripts to avoid snippet declaration conflicts.
    // In Svelte, template snippets like {#snippet filter()} can shadow script imports,
    // but in generated TS they'd conflict at module scope.
    var declared_names: std.StringHashMapUnmanaged(void) = .empty;
    defer declared_names.deinit(allocator);

    if (module_script) |script| {
        const content = ast.source[script.content_start..script.content_end];
        var names = try extractDeclaredNames(allocator, content);
        defer names.deinit(allocator);
        var iter = names.iterator();
        while (iter.next()) |entry| {
            try declared_names.put(allocator, entry.key_ptr.*, {});
        }
    }
    if (instance_script) |script| {
        const content = ast.source[script.content_start..script.content_end];
        var names = try extractDeclaredNames(allocator, content);
        defer names.deinit(allocator);
        var iter = names.iterator();
        while (iter.next()) |entry| {
            try declared_names.put(allocator, entry.key_ptr.*, {});
        }
    }

    // Extract and emit template expressions for type checking
    try emitTemplateExpressions(allocator, &ast, &output, &mappings, declared_names);

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

    // Extract instance exports (methods/values accessible on component instance)
    var instance_exports: std.ArrayList(ExportInfo) = .empty;
    defer instance_exports.deinit(allocator);
    if (instance_script) |script| {
        const content = ast.source[script.content_start..script.content_end];
        try extractInstanceExports(allocator, content, &instance_exports);
    }

    // Emit void statements for $bindable() props to suppress "never read" errors.
    // Bindable props are often only written to (reassigned) in script, not read,
    // but the binding is read by the parent component. TypeScript sees writes but
    // no reads, causing false "declared but its value is never read" errors.
    var has_bindable_props = false;
    for (props.items) |prop| {
        if (prop.is_bindable) {
            if (!has_bindable_props) {
                try output.appendSlice(allocator, "// Mark bindable props as used (read by parent via binding)\n");
                has_bindable_props = true;
            }
            try output.appendSlice(allocator, "void ");
            try output.appendSlice(allocator, prop.name);
            try output.appendSlice(allocator, ";\n");
        }
    }
    if (has_bindable_props) {
        try output.appendSlice(allocator, "\n");
    }

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

    // Generate $$Bindings type for bindable props
    try output.appendSlice(allocator, "export type $$Bindings = ");
    var has_bindable = false;
    for (props.items) |prop| {
        if (prop.is_bindable) {
            if (has_bindable) {
                try output.appendSlice(allocator, " | ");
            }
            try output.appendSlice(allocator, "\"");
            try output.appendSlice(allocator, prop.name);
            try output.appendSlice(allocator, "\"");
            has_bindable = true;
        }
    }
    if (!has_bindable) {
        try output.appendSlice(allocator, "\"\"");
    }
    try output.appendSlice(allocator, ";\n\n");

    // Generate $$Exports interface for instance exports (methods accessible on component)
    // Include index signature for compatibility with Record<string, any> (required by mount())
    try output.appendSlice(allocator, "export interface $$Exports {\n");
    try output.appendSlice(allocator, "  [key: string]: any;\n");
    for (instance_exports.items) |exp| {
        try output.appendSlice(allocator, "  ");
        try output.appendSlice(allocator, exp.name);
        try output.appendSlice(allocator, ": ");
        if (exp.type_repr) |t| {
            try output.appendSlice(allocator, t);
        } else {
            try output.appendSlice(allocator, "any");
        }
        try output.appendSlice(allocator, ";\n");
    }
    try output.appendSlice(allocator, "}\n\n");

    // Generate default export using Component interface (Svelte 5 style)
    // This makes ComponentProps<typeof Component> work correctly.
    // The Component interface has the signature: Component<Props, Exports, Bindings>
    try output.appendSlice(allocator,
        \\declare const __SvelteComponent__: __SvelteComponentType__<$$Props, $$Exports, $$Bindings>;
        \\type __SvelteComponent__ = typeof __SvelteComponent__;
        \\export default __SvelteComponent__;
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

/// Extracts instance exports from Svelte component scripts.
/// These are values exported from the instance script that are accessible on the component instance.
/// Patterns supported:
/// - `export { name }` - re-export of already-defined variable/function
/// - `export { name1, name2 }` - multiple re-exports
/// - `export function name()` - inline function export
/// - `export const name = ...` - inline const export
/// - `export let name = ...` - skipped (these are props in Svelte 4)
fn extractInstanceExports(allocator: std.mem.Allocator, content: []const u8, exports: *std.ArrayList(ExportInfo)) !void {
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

        // Only look for exports at top level
        if (brace_depth == 0 and paren_depth == 0) {
            if (startsWithKeyword(content[i..], "export")) {
                i += 6;
                i = skipWhitespace(content, i);

                // Skip "export let" - these are props, not instance exports
                if (startsWithKeyword(content[i..], "let")) {
                    while (i < content.len and content[i] != ';' and content[i] != '\n') : (i += 1) {}
                    continue;
                }

                // Handle "export { name1, name2 }"
                if (i < content.len and content[i] == '{') {
                    i += 1;
                    while (i < content.len) {
                        i = skipWhitespace(content, i);
                        if (i < content.len and content[i] == '}') {
                            i += 1;
                            break;
                        }

                        // Parse identifier
                        const name_start = i;
                        while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
                        const name = content[name_start..i];

                        if (name.len > 0) {
                            // Look up the type from the function/variable definition
                            const type_repr = findExportedNameType(content, name);
                            try exports.append(allocator, .{
                                .name = name,
                                .type_repr = type_repr,
                            });
                        }

                        i = skipWhitespace(content, i);

                        // Skip "as alias" if present
                        if (startsWithKeyword(content[i..], "as")) {
                            i += 2;
                            i = skipWhitespace(content, i);
                            while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
                        }

                        i = skipWhitespace(content, i);
                        if (i < content.len and content[i] == ',') {
                            i += 1;
                        }
                    }
                    continue;
                }

                // Handle "export function name(...): ReturnType"
                // Use "typeof name" to reference the function's inferred type from the script
                if (startsWithKeyword(content[i..], "function")) {
                    i += 8;
                    i = skipWhitespace(content, i);

                    const name_start = i;
                    while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
                    const name = content[name_start..i];

                    if (name.len > 0) {
                        // Use typeof to reference the actual function's type
                        // This handles default parameters, generics, overloads correctly
                        const type_repr = try std.fmt.allocPrint(allocator, "typeof {s}", .{name});
                        try exports.append(allocator, .{
                            .name = name,
                            .type_repr = type_repr,
                        });
                    }

                    // Skip to end of function body
                    while (i < content.len and content[i] != '{') : (i += 1) {}
                    if (i < content.len) {
                        var body_depth: u32 = 0;
                        while (i < content.len) {
                            if (content[i] == '{') body_depth += 1;
                            if (content[i] == '}') {
                                body_depth -= 1;
                                if (body_depth == 0) {
                                    i += 1;
                                    break;
                                }
                            }
                            i += 1;
                        }
                    }
                    continue;
                }

                // Handle "export const name = ..." or "export const name: Type = ..."
                if (startsWithKeyword(content[i..], "const")) {
                    i += 5;
                    i = skipWhitespace(content, i);

                    const name_start = i;
                    while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
                    const name = content[name_start..i];

                    if (name.len > 0) {
                        i = skipWhitespace(content, i);

                        var type_repr: ?[]const u8 = null;
                        if (i < content.len and content[i] == ':') {
                            i += 1;
                            i = skipWhitespace(content, i);
                            const type_start = i;
                            i = skipTypeAnnotation(content, i);
                            type_repr = std.mem.trim(u8, content[type_start..i], " \t\n\r");
                        }

                        try exports.append(allocator, .{
                            .name = name,
                            .type_repr = type_repr,
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

/// Find the type of an exported name by looking up its definition in the script.
/// Returns the function signature or variable type if found.
fn findExportedNameType(content: []const u8, name: []const u8) ?[]const u8 {
    // Look for "function name(" pattern
    var search_i: usize = 0;
    while (search_i + 8 + name.len < content.len) {
        if (std.mem.startsWith(u8, content[search_i..], "function")) {
            const after_func = search_i + 8;
            var j = after_func;
            // Skip whitespace
            while (j < content.len and std.ascii.isWhitespace(content[j])) : (j += 1) {}
            // Check if name matches
            if (j + name.len <= content.len and std.mem.eql(u8, content[j .. j + name.len], name)) {
                const after_name = j + name.len;
                // Make sure it's not a prefix of another identifier
                if (after_name >= content.len or !isIdentChar(content[after_name])) {
                    // Found function definition - for now return "any" as extracting full signature is complex
                    // The function is already emitted in the script content, so TypeScript will infer
                    return null;
                }
            }
        }
        search_i += 1;
    }

    return null;
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
    // Find the start of the let statement (may span multiple lines for multi-line destructuring)
    var let_pos: ?usize = null;
    var search_pos: usize = props_call;
    while (search_pos > 0) {
        search_pos -= 1;
        if (search_pos + 3 <= content.len and startsWithKeyword(content[search_pos..], "let")) {
            let_pos = search_pos;
            break;
        }
        // Only break on semicolon (newlines are fine for multi-line destructuring)
        if (content[search_pos] == ';') break;
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
                // Skip generic type parameter <...> if present (e.g., $bindable<T>(...))
                if (i < content.len and content[i] == '<') {
                    var angle_depth: u32 = 1;
                    i += 1;
                    while (i < content.len and angle_depth > 0) {
                        if (content[i] == '<') angle_depth += 1;
                        if (content[i] == '>') angle_depth -= 1;
                        i += 1;
                    }
                    i = skipWhitespace(content, i);
                }
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
            i = skipPropsTypeAnnotation(content, i);
            const interface_name = std.mem.trim(u8, content[type_start..i], " \t\n\r");
            if (interface_name.len > 0) {
                return interface_name;
            }
        }
    }

    return null;
}

/// Skip a complete TypeScript type annotation for $props(), handling:
/// - Simple identifiers (Props)
/// - Generic types (Pick<T, K>)
/// - Intersection types (A & B)
/// - Union types (A | B)
/// - Inline object types ({ foo: bar })
/// - Function types with arrow (=> void)
/// - Multi-line types (newlines allowed inside brackets)
/// Stops at = when at depth 0 (for the `= $props()` part).
fn skipPropsTypeAnnotation(content: []const u8, start: usize) usize {
    var i = start;
    var angle_depth: u32 = 0; // < >
    var brace_depth: u32 = 0; // { }
    var bracket_depth: u32 = 0; // [ ]
    var paren_depth: u32 = 0; // ( )

    while (i < content.len) {
        const c = content[i];

        // Handle arrow in function types (=>) - must check before = handling
        if (c == '=' and i + 1 < content.len and content[i + 1] == '>') {
            i += 2; // Skip both = and >
            continue;
        }

        // Track depths separately for proper bracket matching
        if (c == '<') {
            angle_depth += 1;
            i += 1;
            continue;
        }
        if (c == '>') {
            if (angle_depth > 0) {
                angle_depth -= 1;
                i += 1;
                continue;
            }
            // Standalone > not after = - could be comparison or end of type
            // In type context inside braces, continue; otherwise break
            if (brace_depth > 0 or paren_depth > 0 or bracket_depth > 0) {
                i += 1;
                continue;
            }
            break;
        }
        if (c == '{') {
            brace_depth += 1;
            i += 1;
            continue;
        }
        if (c == '}') {
            if (brace_depth > 0) {
                brace_depth -= 1;
                i += 1;
                continue;
            }
            // Unbalanced } - end of type
            break;
        }
        if (c == '[') {
            bracket_depth += 1;
            i += 1;
            continue;
        }
        if (c == ']') {
            if (bracket_depth > 0) {
                bracket_depth -= 1;
                i += 1;
                continue;
            }
            break;
        }
        if (c == '(') {
            paren_depth += 1;
            i += 1;
            continue;
        }
        if (c == ')') {
            if (paren_depth > 0) {
                paren_depth -= 1;
                i += 1;
                continue;
            }
            break;
        }

        // Handle strings (for literal types like 'icon' | 'label')
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(content, i);
            continue;
        }

        const total_depth = angle_depth + brace_depth + bracket_depth + paren_depth;

        // Stop at = when at depth 0 (start of = $props())
        if (total_depth == 0 and c == '=') {
            break;
        }

        i += 1;
    }

    return i;
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

/// Checks if a file uses Snippet types (for conditional import).
/// Returns true if:
/// - The AST contains {#snippet} blocks
/// - Script content contains "Snippet" as a type reference (not just substring)
fn fileUsesSnippet(ast: *const Ast) bool {
    // Check for {#snippet} blocks in template
    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) return true;
    }

    // Check for Snippet type references in scripts
    for (ast.scripts.items) |script| {
        const content = ast.source[script.content_start..script.content_end];
        // Look for "Snippet" as a word boundary (not part of another identifier)
        var i: usize = 0;
        while (i + 7 <= content.len) {
            if (std.mem.startsWith(u8, content[i..], "Snippet")) {
                // Check it's not part of a larger identifier
                const before_ok = i == 0 or !std.ascii.isAlphanumeric(content[i - 1]) and content[i - 1] != '_';
                const after_ok = i + 7 >= content.len or !std.ascii.isAlphanumeric(content[i + 7]) and content[i + 7] != '_';
                if (before_ok and after_ok) return true;
            }
            i += 1;
        }
    }

    return false;
}

const ExprInfo = struct {
    expr: []const u8,
    offset: u32,
};

const EachBindingInfo = struct {
    iterable: []const u8,
    item_binding: []const u8,
    index_binding: ?[]const u8,
    key_expr: ?[]const u8, // Key expression from (key) in keyed each
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

const SnippetNameInfo = struct {
    name: []const u8,
    params: ?[]const u8,
    offset: u32,
};

/// Extracts all declared names from script content (imports and top-level declarations).
/// Used to avoid emitting snippet declarations that would conflict with script bindings.
/// In Svelte, template snippets can shadow script imports (scoped), but in our generated
/// TypeScript they'd conflict since everything is at module scope.
fn extractDeclaredNames(allocator: std.mem.Allocator, content: []const u8) !std.StringHashMapUnmanaged(void) {
    var names: std.StringHashMapUnmanaged(void) = .empty;

    var i: usize = 0;
    while (i < content.len) {
        // Skip whitespace
        while (i < content.len and std.ascii.isWhitespace(content[i])) : (i += 1) {}
        if (i >= content.len) break;

        // Skip comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            continue;
        }
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '*') {
            i += 2;
            while (i + 1 < content.len and !(content[i] == '*' and content[i + 1] == '/')) : (i += 1) {}
            if (i + 1 < content.len) i += 2;
            continue;
        }

        // Check for import statements
        if (std.mem.startsWith(u8, content[i..], "import ")) {
            // Find the import specifiers: { name1, name2 } or name
            const line_start = i;
            var line_end = i;
            while (line_end < content.len and content[line_end] != '\n') : (line_end += 1) {}
            const line = content[line_start..line_end];

            // Named imports: import { a, b, type c } from '...'
            if (std.mem.indexOf(u8, line, "{")) |brace_start| {
                if (std.mem.indexOf(u8, line, "}")) |brace_end| {
                    if (brace_start < brace_end) {
                        const specifiers = line[brace_start + 1 .. brace_end];
                        var spec_iter = std.mem.splitScalar(u8, specifiers, ',');
                        while (spec_iter.next()) |spec_raw| {
                            var spec = std.mem.trim(u8, spec_raw, " \t\r\n");
                            if (spec.len == 0) continue;

                            // Skip "type X" imports
                            if (std.mem.startsWith(u8, spec, "type ")) continue;

                            // Handle "X as Y" - extract Y
                            if (std.mem.indexOf(u8, spec, " as ")) |as_pos| {
                                spec = std.mem.trim(u8, spec[as_pos + 4 ..], " \t");
                            }

                            // Extract identifier (stop at non-alphanumeric)
                            var name_end: usize = 0;
                            while (name_end < spec.len and (std.ascii.isAlphanumeric(spec[name_end]) or spec[name_end] == '_' or spec[name_end] == '$')) : (name_end += 1) {}
                            if (name_end > 0) {
                                try names.put(allocator, spec[0..name_end], {});
                            }
                        }
                    }
                }
            }

            // Namespace imports: import * as name from '...'
            if (std.mem.indexOf(u8, line, "* as ")) |as_start| {
                const after_as = line[as_start + 5 ..];
                const trimmed = std.mem.trim(u8, after_as, " \t");
                var name_end: usize = 0;
                while (name_end < trimmed.len and (std.ascii.isAlphanumeric(trimmed[name_end]) or trimmed[name_end] == '_' or trimmed[name_end] == '$')) : (name_end += 1) {}
                if (name_end > 0) {
                    try names.put(allocator, trimmed[0..name_end], {});
                }
            }

            // Default imports: import name from '...' (but not import { ... } or import * as)
            if (std.mem.indexOf(u8, line, "{") == null and std.mem.indexOf(u8, line, "*") == null) {
                const after_import = std.mem.trim(u8, line["import ".len..], " \t");
                // Skip 'type' keyword if present
                const after_type = if (std.mem.startsWith(u8, after_import, "type "))
                    std.mem.trim(u8, after_import["type ".len..], " \t")
                else
                    after_import;
                // Extract the name before 'from'
                if (std.mem.indexOf(u8, after_type, " from")) |from_pos| {
                    const name_part = std.mem.trim(u8, after_type[0..from_pos], " \t");
                    var name_end: usize = 0;
                    while (name_end < name_part.len and (std.ascii.isAlphanumeric(name_part[name_end]) or name_part[name_end] == '_' or name_part[name_end] == '$')) : (name_end += 1) {}
                    if (name_end > 0) {
                        try names.put(allocator, name_part[0..name_end], {});
                    }
                }
            }

            i = line_end;
            continue;
        }

        i += 1;
    }

    return names;
}

/// Emits type declarations for generic parameters from Svelte 5 generics="..." attribute.
/// Parses comma-separated generic parameters and emits each as a type declaration.
/// Examples:
/// - "T" → "type T = unknown;"
/// - "T extends Foo" → "type T = Foo;"
/// - "T, U" → "type T = unknown;\ntype U = unknown;"
/// - "T extends Foo, U extends Bar" → "type T = Foo;\ntype U = Bar;"
fn emitGenericTypeDeclarations(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    generics: []const u8,
) !void {
    var i: usize = 0;

    while (i < generics.len) {
        // Skip whitespace
        while (i < generics.len and std.ascii.isWhitespace(generics[i])) : (i += 1) {}
        if (i >= generics.len) break;

        // Parse generic parameter name
        const name_start = i;
        while (i < generics.len and std.ascii.isAlphanumeric(generics[i])) : (i += 1) {}
        const name = generics[name_start..i];

        if (name.len == 0) {
            // Skip unexpected characters
            i += 1;
            continue;
        }

        // Skip whitespace
        while (i < generics.len and std.ascii.isWhitespace(generics[i])) : (i += 1) {}

        // Check for "extends" constraint
        var constraint: []const u8 = "unknown";
        if (i + 7 <= generics.len and std.mem.eql(u8, generics[i .. i + 7], "extends")) {
            i += 7;
            // Skip whitespace after "extends"
            while (i < generics.len and std.ascii.isWhitespace(generics[i])) : (i += 1) {}

            // Parse constraint type (until comma, = for default, or end, handling < > for generics)
            const constraint_start = i;
            var angle_depth: u32 = 0;
            while (i < generics.len) {
                const c = generics[i];
                if (c == '<') {
                    angle_depth += 1;
                } else if (c == '>') {
                    if (angle_depth > 0) angle_depth -= 1;
                } else if ((c == ',' or c == '=') and angle_depth == 0) {
                    break;
                }
                i += 1;
            }
            constraint = std.mem.trim(u8, generics[constraint_start..i], " \t\n\r");

            // Skip default type parameter if present (= DefaultType)
            if (i < generics.len and generics[i] == '=') {
                i += 1;
                // Skip the default value (until comma or end, handling < >)
                angle_depth = 0;
                while (i < generics.len) {
                    const c = generics[i];
                    if (c == '<') {
                        angle_depth += 1;
                    } else if (c == '>') {
                        if (angle_depth > 0) angle_depth -= 1;
                    } else if (c == ',' and angle_depth == 0) {
                        break;
                    }
                    i += 1;
                }
            }
        }

        // Emit type declaration
        try output.appendSlice(allocator, "type ");
        try output.appendSlice(allocator, name);
        try output.appendSlice(allocator, " = ");
        try output.appendSlice(allocator, constraint);
        try output.appendSlice(allocator, ";\n");

        // Skip comma if present
        while (i < generics.len and std.ascii.isWhitespace(generics[i])) : (i += 1) {}
        if (i < generics.len and generics[i] == ',') {
            i += 1;
        }
    }
}

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
///
/// `declared_names` contains names already declared in the script (imports, variables).
/// Snippet declarations for these names are skipped to avoid conflicts. In Svelte,
/// template snippets create a new scope and can shadow script bindings, but in our
/// generated TypeScript everything is at module scope.
fn emitTemplateExpressions(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    output: *std.ArrayList(u8),
    _: *std.ArrayList(SourceMap.Mapping),
    declared_names: std.StringHashMapUnmanaged(void),
) !void {
    var has_expressions = false;

    // Collect identifiers referenced in template expressions to emit void statements.
    // This marks variables as "used" so noUnusedLocals works correctly.
    var template_refs: std.StringHashMapUnmanaged(void) = .empty;
    defer template_refs.deinit(allocator);

    for (ast.nodes.items) |node| {
        switch (node.kind) {
            .snippet => {
                if (extractSnippetName(ast.source, node.start, node.end)) |info| {
                    // Skip snippet declarations for names already declared in script.
                    // In Svelte, {#snippet filter()} can shadow imported `filter`,
                    // but in generated TS they'd conflict at module scope.
                    if (declared_names.contains(info.name)) continue;

                    if (!has_expressions) {
                        try output.appendSlice(allocator, "// Template expressions\n");
                        has_expressions = true;
                    }

                    // Emit snippet variable declaration: var snippetName: any = null;
                    try output.appendSlice(allocator, "var ");
                    try output.appendSlice(allocator, info.name);
                    try output.appendSlice(allocator, ": any = null;\n");

                    // Emit param declarations if present
                    if (info.params) |params| {
                        try emitSnippetParamDeclarations(allocator, output, params);
                    }
                }
            },

            .expression, .if_block, .await_block, .key_block, .render, .html, .const_tag, .debug_tag => {
                // Extract identifiers from template expressions
                const expr = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, expr, &template_refs);
            },

            .each_block => {
                // Extract identifiers from each expression and emit binding declarations
                const expr = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, expr, &template_refs);

                // Parse and emit each block bindings (item, index variables)
                if (extractEachBindings(ast.source, node.start, node.end)) |binding| {
                    if (!has_expressions) {
                        try output.appendSlice(allocator, "// Template expressions\n");
                        has_expressions = true;
                    }
                    try emitEachBindingDeclarations(allocator, output, binding, binding.iterable);
                }
            },

            .element, .component => {
                // Extract identifiers from attribute expressions
                const elem_data = ast.elements.items[node.data];

                // For components, extract the component name (or namespace for Namespace.Component)
                if (node.kind == .component) {
                    const tag = elem_data.tag_name;
                    // Extract first identifier (e.g., "Table" from "Table.Root")
                    var name_end: usize = 0;
                    while (name_end < tag.len and (std.ascii.isAlphanumeric(tag[name_end]) or tag[name_end] == '_' or tag[name_end] == '$')) : (name_end += 1) {}
                    if (name_end > 0) {
                        try template_refs.put(allocator, tag[0..name_end], {});
                    }
                }

                for (ast.attributes.items[elem_data.attrs_start..elem_data.attrs_end]) |attr| {
                    if (attr.value) |val| {
                        // Expression values are wrapped in {}
                        if (val.len > 0 and val[0] == '{') {
                            try extractIdentifiersFromExpr(allocator, val, &template_refs);
                        }
                    }
                    // Shorthand attributes like {foo} use attr name as expression
                    if (attr.value == null and attr.name.len > 0 and !std.mem.startsWith(u8, attr.name, "on:") and !std.mem.startsWith(u8, attr.name, "bind:")) {
                        // This might be a shorthand {foo}
                        const src = ast.source[attr.start..attr.end];
                        if (src.len > 2 and src[0] == '{' and src[src.len - 1] == '}') {
                            try extractIdentifiersFromExpr(allocator, src, &template_refs);
                        }
                    }
                    // Shorthand bind:x directive (bind:open means bind:open={open})
                    if (attr.value == null and std.mem.startsWith(u8, attr.name, "bind:")) {
                        const binding_name = attr.name[5..]; // Skip "bind:"
                        if (binding_name.len > 0) {
                            try template_refs.put(allocator, binding_name, {});
                        }
                    }
                    // Transition, animate, and use directives reference the function name
                    // e.g., transition:fade, in:fly, out:slide, use:enhance
                    if (attr.value == null) {
                        for ([_][]const u8{ "transition:", "in:", "out:", "animate:", "use:" }) |prefix| {
                            if (std.mem.startsWith(u8, attr.name, prefix)) {
                                // Extract function name (may have | modifiers like fade|local)
                                const rest = attr.name[prefix.len..];
                                var end: usize = 0;
                                while (end < rest.len and (std.ascii.isAlphanumeric(rest[end]) or rest[end] == '_')) : (end += 1) {}
                                if (end > 0) {
                                    try template_refs.put(allocator, rest[0..end], {});
                                }
                                break;
                            }
                        }
                    }
                    // class: directive - shorthand class:foo means class:foo={foo}
                    // e.g., class:hidden, class:active, class:animate
                    if (std.mem.startsWith(u8, attr.name, "class:")) {
                        const class_var = attr.name[6..]; // Skip "class:"
                        if (class_var.len > 0) {
                            if (attr.value) |val| {
                                // Full syntax: class:foo={expr} - extract identifiers from expr
                                try extractIdentifiersFromExpr(allocator, val, &template_refs);
                            } else {
                                // Shorthand: class:foo uses variable named foo
                                try template_refs.put(allocator, class_var, {});
                            }
                        }
                    }
                    // style: directive - shorthand style:prop means style:prop={prop}
                    // e.g., style:color, style:--custom-property
                    if (std.mem.startsWith(u8, attr.name, "style:")) {
                        if (attr.value) |val| {
                            // Full syntax: style:prop={expr} - extract identifiers from expr
                            try extractIdentifiersFromExpr(allocator, val, &template_refs);
                        }
                        // Note: shorthand style:foo (without value) is rarely used with variables
                        // because style properties are usually CSS names, not JS identifiers
                    }
                }
            },

            else => {},
        }
    }

    // Fallback: scan template source directly for {#each} patterns not captured by AST
    // The parser sometimes misses {#each} blocks inside component elements
    try scanTemplateForEachBlocks(allocator, ast, output, &template_refs, &has_expressions);

    // Emit void statements for template-referenced identifiers
    // This marks them as "used" for noUnusedLocals checking
    var iter = template_refs.keyIterator();
    while (iter.next()) |key| {
        const name = key.*;
        // Skip keywords and built-ins, UNLESS the name is declared in script.
        // User imports can shadow built-in names (e.g., `import Map from './icons/map'`),
        // so if a name is declared in script, we should emit void for it.
        if (isJsKeywordOrBuiltin(name) and !declared_names.contains(name)) continue;

        if (!has_expressions) {
            try output.appendSlice(allocator, "// Template expressions\n");
            has_expressions = true;
        }
        try output.appendSlice(allocator, "void ");
        try output.appendSlice(allocator, name);
        try output.appendSlice(allocator, ";\n");
    }

    if (has_expressions) {
        try output.appendSlice(allocator, "\n");
    }
}

/// Extracts identifier references from a template expression string.
/// Handles expressions like {foo}, {foo.bar}, {foo + bar}, onclick={handler}, etc.
fn extractIdentifiersFromExpr(
    allocator: std.mem.Allocator,
    expr: []const u8,
    refs: *std.StringHashMapUnmanaged(void),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < expr.len) {
        const c = expr[i];

        // Handle strings - for template literals, extract from interpolations
        if (c == '"' or c == '\'') {
            i = skipStringLiteral(expr, i);
            continue;
        }
        if (c == '`') {
            i = try skipStringLiteralAndExtract(allocator, expr, i, refs);
            continue;
        }

        // Skip comments
        if (i + 1 < expr.len and c == '/') {
            if (expr[i + 1] == '/') {
                while (i < expr.len and expr[i] != '\n') : (i += 1) {}
                continue;
            }
            if (expr[i + 1] == '*') {
                i += 2;
                while (i + 1 < expr.len and !(expr[i] == '*' and expr[i + 1] == '/')) : (i += 1) {}
                if (i + 1 < expr.len) i += 2;
                continue;
            }
        }

        // Check for identifier start
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            const start = i;
            while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '$')) : (i += 1) {}
            const ident = expr[start..i];

            // Skip if preceded by dot (member access) - but NOT spread syntax (...)
            // Member access: obj.prop (single dot before identifier)
            // Spread syntax: ...props (three dots before identifier) - keep these!
            if (start > 0 and expr[start - 1] == '.') {
                // Check if this is spread syntax (three consecutive dots)
                const is_spread = start >= 3 and
                    expr[start - 1] == '.' and
                    expr[start - 2] == '.' and
                    expr[start - 3] == '.';
                if (!is_spread) continue; // Skip member access, but keep spread
            }

            // Store identifier reference
            if (ident.len > 0) {
                try refs.put(allocator, ident, {});

                // For $prefixed identifiers (Svelte store subscriptions like $showNudge),
                // also add the unprefixed name (showNudge). The store variable is declared
                // in script, but only the $prefixed form is used in templates. Adding both
                // ensures the original variable is marked as "used".
                if (ident[0] == '$' and ident.len > 1) {
                    try refs.put(allocator, ident[1..], {});
                }
            }
            continue;
        }

        i += 1;
    }
}

/// Skips a string literal starting at position i, returning position after closing quote.
/// For template literals, also extracts identifiers from interpolations.
fn skipStringLiteralAndExtract(
    allocator: std.mem.Allocator,
    expr: []const u8,
    start: usize,
    refs: *std.StringHashMapUnmanaged(void),
) std.mem.Allocator.Error!usize {
    if (start >= expr.len) return start;
    const quote = expr[start];
    var i = start + 1;
    while (i < expr.len) {
        if (expr[i] == '\\' and i + 1 < expr.len) {
            i += 2;
            continue;
        }
        if (expr[i] == quote) {
            return i + 1;
        }
        // Template literal interpolation - extract identifiers from inside
        if (quote == '`' and expr[i] == '$' and i + 1 < expr.len and expr[i + 1] == '{') {
            const interp_start = i + 2;
            i += 2;
            var depth: u32 = 1;
            while (i < expr.len and depth > 0) {
                if (expr[i] == '{') depth += 1;
                if (expr[i] == '}') depth -= 1;
                if (depth > 0) i += 1;
            }
            const interp_end = i;
            // Recursively extract identifiers from the interpolation content
            try extractIdentifiersFromExpr(allocator, expr[interp_start..interp_end], refs);
            if (i < expr.len) i += 1; // skip closing }
            continue;
        }
        i += 1;
    }
    return i;
}

/// Simple string skip for non-template literals (no identifier extraction needed)
fn skipStringLiteral(expr: []const u8, start: usize) usize {
    if (start >= expr.len) return start;
    const quote = expr[start];
    var i = start + 1;
    while (i < expr.len) {
        if (expr[i] == '\\' and i + 1 < expr.len) {
            i += 2;
            continue;
        }
        if (expr[i] == quote) {
            return i + 1;
        }
        i += 1;
    }
    return i;
}

/// Finds the position after the matching closing brace, starting from the position
/// after an opening brace. Properly handles nested braces and skips over string
/// literals (single, double, and template) to avoid counting braces inside strings.
fn findMatchingCloseBrace(template: []const u8, start: usize) usize {
    var depth: u32 = 1;
    var i = start;
    while (i < template.len and depth > 0) {
        const c = template[i];
        // Skip string literals - braces inside strings shouldn't affect depth
        if (c == '"' or c == '\'' or c == '`') {
            i = skipStringLiteral(template, i);
            continue;
        }
        if (c == '{') depth += 1;
        if (c == '}') depth -= 1;
        i += 1;
    }
    return i;
}

/// Scans template source directly for template expressions not captured by AST nodes.
/// This is a fallback for when the parser misses expressions inside component elements.
fn scanTemplateForEachBlocks(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    output: *std.ArrayList(u8),
    template_refs: *std.StringHashMapUnmanaged(void),
    has_expressions: *bool,
) !void {
    // Find template portion (after scripts, before styles)
    var template_start: usize = 0;
    var template_end: usize = ast.source.len;

    // Skip script blocks to find template
    for (ast.scripts.items) |script| {
        // Find </script> after this script's content
        if (std.mem.indexOf(u8, ast.source[script.content_end..], "</script>")) |end_offset| {
            const script_block_end = script.content_end + end_offset + 9;
            if (script_block_end > template_start) {
                template_start = script_block_end;
            }
        }
    }

    // Find top-level <style> tag to limit template_end
    // Must be at the start of a line with NO indentation (truly top-level)
    // <style> inside <svelte:head> or other blocks will be indented
    // The component's own <style> block is at column 0
    const source_slice = ast.source[template_start..];
    var search_pos: usize = 0;
    while (search_pos < source_slice.len) {
        if (std.mem.indexOf(u8, source_slice[search_pos..], "<style")) |rel_offset| {
            const style_pos = search_pos + rel_offset;
            // Check if it's at column 0 (preceded by newline only, no whitespace)
            var is_top_level = style_pos == 0; // At start of template
            if (!is_top_level and style_pos > 0) {
                // Top-level <style> must be immediately after newline
                is_top_level = source_slice[style_pos - 1] == '\n';
            }
            if (is_top_level) {
                template_end = template_start + style_pos;
                break;
            }
            search_pos = style_pos + 6; // Skip past this "<style" and keep searching
        } else {
            break;
        }
    }

    const template = ast.source[template_start..template_end];

    // Scan for ALL {expression} patterns (including {#if}, {#each}, {@render}, plain {expr})
    var i: usize = 0;
    while (i < template.len) {
        if (template[i] != '{') {
            i += 1;
            continue;
        }

        const expr_start = i;

        // Find matching closing brace, respecting nesting and string literals
        const j = findMatchingCloseBrace(template, i + 1);

        const full_expr = template[expr_start..j];

        // Extract identifiers from the expression
        try extractIdentifiersFromExpr(allocator, full_expr, template_refs);

        // Special handling for {#each} - emit binding declarations
        if (std.mem.startsWith(u8, full_expr, "{#each ")) {
            if (extractEachBindings(template, @intCast(expr_start), @intCast(j))) |binding| {
                if (!has_expressions.*) {
                    try output.appendSlice(allocator, "// Template expressions\n");
                    has_expressions.* = true;
                }
                try emitEachBindingDeclarations(allocator, output, binding, binding.iterable);
            }
        }

        // Special handling for {@const} - emit variable declaration
        if (std.mem.startsWith(u8, full_expr, "{@const ")) {
            if (extractConstBinding(template, @intCast(expr_start), @intCast(j))) |binding| {
                if (!has_expressions.*) {
                    try output.appendSlice(allocator, "// Template expressions\n");
                    has_expressions.* = true;
                }
                try output.appendSlice(allocator, "var ");
                try output.appendSlice(allocator, binding.name);
                try output.appendSlice(allocator, " = ");
                try output.appendSlice(allocator, binding.expr);
                try output.appendSlice(allocator, ";\n");
            }
        }

        i = j; // move past this expression
    }

    // Scan for component tags (capitalized tag names)
    try scanTemplateForComponents(allocator, template, template_refs);
}

/// Scans template source for component usages (capitalized tag names) and
/// attribute expressions. Fallback for when the parser doesn't detect elements
/// inside block structures.
fn scanTemplateForComponents(
    allocator: std.mem.Allocator,
    template: []const u8,
    refs: *std.StringHashMapUnmanaged(void),
) !void {
    var i: usize = 0;
    while (i < template.len) {
        if (template[i] == '<' and i + 1 < template.len) {
            const next_char = template[i + 1];
            // Skip closing tags, comments, special tags
            if (next_char == '/' or next_char == '!' or next_char == '?') {
                i += 1;
                continue;
            }

            // Check for valid tag start (letter for regular tags)
            if (std.ascii.isAlphabetic(next_char)) {
                // Check for uppercase letter (component)
                if (next_char >= 'A' and next_char <= 'Z') {
                    // Extract component name (up to space, >, /, or .)
                    var name_end = i + 2;
                    while (name_end < template.len) {
                        const c = template[name_end];
                        if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                            name_end += 1;
                        } else {
                            break;
                        }
                    }
                    const name = template[i + 1 .. name_end];
                    if (name.len > 0) {
                        try refs.put(allocator, name, {});
                    }
                    i = name_end;
                } else {
                    // Lowercase tag - skip the tag name
                    i += 2;
                    while (i < template.len and (std.ascii.isAlphanumeric(template[i]) or template[i] == '-' or template[i] == ':')) : (i += 1) {}
                }

                // Scan tag attributes for expressions (until > or />)
                while (i < template.len and template[i] != '>') {
                    // Look for { in attributes
                    if (template[i] == '{') {
                        const expr_start = i;
                        // Find matching brace, skipping strings to handle braces in string content
                        i = findMatchingCloseBrace(template, i + 1);
                        // Extract identifiers from attribute expression (includes spreads like {...props})
                        const attr_expr = template[expr_start..i];
                        try extractIdentifiersFromExpr(allocator, attr_expr, refs);
                    } else if (template[i] == '/' and i + 1 < template.len and template[i + 1] == '>') {
                        i += 2; // Skip />
                        break;
                    } else if (std.mem.startsWith(u8, template[i..], "bind:")) {
                        // Detect bind:xxx shorthand (bind:open means bind:open={open})
                        i += 5; // Skip "bind:"
                        const name_start = i;
                        while (i < template.len and (std.ascii.isAlphanumeric(template[i]) or template[i] == '_' or template[i] == '$')) : (i += 1) {}
                        const binding_name = template[name_start..i];
                        if (binding_name.len > 0) {
                            // Skip whitespace
                            while (i < template.len and (template[i] == ' ' or template[i] == '\t')) : (i += 1) {}
                            // Check if there's no = (shorthand) or next char is space/> (end of attr)
                            if (i >= template.len or template[i] != '=') {
                                // Shorthand bind:xxx - xxx is used as identifier
                                try refs.put(allocator, binding_name, {});
                            }
                            // If there's =, the { } will be caught by the expression handler above
                        }
                    } else if (detectDirectiveIdentifier(template, i)) |directive_info| {
                        // Detect Svelte directives that reference identifiers:
                        // transition:fade, in:fly, out:slide, animate:flip, use:action
                        try refs.put(allocator, directive_info.name, {});
                        i = directive_info.end_pos;
                    } else {
                        i += 1;
                    }
                }
                continue;
            }
        }
        i += 1;
    }
}

const DirectiveInfo = struct {
    name: []const u8,
    end_pos: usize,
};

/// Detects Svelte directives that reference identifiers:
/// transition:xxx, in:xxx, out:xxx, animate:xxx, use:xxx
/// Returns the identifier name and position after it, or null if not a directive.
fn detectDirectiveIdentifier(template: []const u8, pos: usize) ?DirectiveInfo {
    const directives = [_][]const u8{ "transition:", "animate:", "use:", "in:", "out:" };

    for (directives) |prefix| {
        if (pos + prefix.len < template.len and std.mem.startsWith(u8, template[pos..], prefix)) {
            const name_start = pos + prefix.len;
            var name_end = name_start;
            // Extract identifier (alphanumeric, underscore, dollar)
            while (name_end < template.len) {
                const c = template[name_end];
                if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                    name_end += 1;
                } else {
                    break;
                }
            }
            if (name_end > name_start) {
                return .{
                    .name = template[name_start..name_end],
                    .end_pos = name_end,
                };
            }
        }
    }
    return null;
}

/// Returns true if the identifier is a JavaScript keyword or built-in.
fn isJsKeywordOrBuiltin(name: []const u8) bool {
    const keywords = [_][]const u8{
        // JS keywords
        "if",        "else",       "for",        "while",     "do",      "switch",
        "case",      "default",    "break",      "continue",  "return",  "throw",
        "try",       "catch",      "finally",    "new",       "delete",  "typeof",
        "void",      "in",         "instanceof", "this",      "class",   "extends",
        "super",     "import",     "export",     "from",      "as",      "async",
        "await",     "yield",      "let",        "const",     "var",     "function",
        "true",      "false",      "null",       "undefined", "NaN",     "Infinity",
        // Reserved keywords (strict mode and future reserved)
        "with",      "enum",       "implements", "interface", "package", "private",
        "protected", "public",     "static",
        // Built-in objects
            "Array",     "Object",  "String",
        "Number",    "Boolean",    "Symbol",     "BigInt",    "Math",    "Date",
        "RegExp",    "Error",      "JSON",       "Promise",   "Map",     "Set",
        "WeakMap",   "WeakSet",    "Proxy",      "Reflect",   "console", "window",
        "document",  "globalThis",
        // Svelte keywords in template blocks
        "each",       "snippet",   "render",  "html",
        "debug",     "key",
    };
    for (keywords) |kw| {
        if (std.mem.eql(u8, name, kw)) return true;
    }
    return false;
}

/// Emits variable declarations for each block bindings
fn emitEachBindingDeclarations(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    binding: EachBindingInfo,
    iterable: []const u8,
) !void {
    // Emit item binding: var item = ((iterable) ?? [])[0];
    // Using var allows redeclaration for multiple blocks with same name
    // The ?? [] fallback handles null/undefined iterables (Svelte treats them as empty)
    // Also transform [...x] spreads inside the iterable to [...(x ?? [])] for null safety
    const safe_iterable = try makeIterableNullSafe(allocator, iterable);

    try output.appendSlice(allocator, "var ");
    try output.appendSlice(allocator, binding.item_binding);
    try output.appendSlice(allocator, " = ((");
    try output.appendSlice(allocator, safe_iterable);
    try output.appendSlice(allocator, ") ?? [])[0];\n");

    // Emit index binding if present: var i = 0;
    if (binding.index_binding) |idx| {
        try output.appendSlice(allocator, "var ");
        try output.appendSlice(allocator, idx);
        try output.appendSlice(allocator, " = 0;\n");
    }

    // Emit void statement for key expression to mark it as used.
    // This handles cases like {#each items as [key, value] (key)} where key
    // is only used in the key expression, not in the loop body.
    if (binding.key_expr) |key| {
        try output.appendSlice(allocator, "void (");
        try output.appendSlice(allocator, key);
        try output.appendSlice(allocator, ");\n");
    }
}

/// Transforms spread expressions inside an iterable to be null-safe.
/// Replaces `[...x]` with `[...(x ?? [])]` to handle nullable arrays.
/// Svelte tolerates null/undefined in {#each}, treating them as empty arrays.
fn makeIterableNullSafe(allocator: std.mem.Allocator, iterable: []const u8) ![]const u8 {
    // Look for [...identifier] or [...expression] patterns and wrap with ?? []
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);
    try result.ensureTotalCapacity(allocator, iterable.len + 32);

    var i: usize = 0;
    while (i < iterable.len) {
        // Look for "[..." pattern
        if (i + 4 <= iterable.len and std.mem.eql(u8, iterable[i .. i + 4], "[...")) {
            try result.appendSlice(allocator, "[...(");
            i += 4;

            // Find the matching ] by tracking bracket depth
            const spread_start = i;
            var bracket_depth: u32 = 1;
            var paren_depth: u32 = 0;

            while (i < iterable.len and bracket_depth > 0) {
                const c = iterable[i];
                switch (c) {
                    '[' => bracket_depth += 1,
                    ']' => bracket_depth -= 1,
                    '(' => paren_depth += 1,
                    ')' => {
                        if (paren_depth > 0) paren_depth -= 1;
                    },
                    else => {},
                }
                if (bracket_depth > 0) i += 1;
            }

            // i now points to the closing ]
            const spread_expr = iterable[spread_start..i];

            // Check if it's a method call like .reverse() - extract just the base
            // For [...suggestions].reverse(), spread_expr is "suggestions"
            try result.appendSlice(allocator, spread_expr);
            try result.appendSlice(allocator, " ?? [])");

            // Copy the closing ]
            if (i < iterable.len and iterable[i] == ']') {
                try result.append(allocator, ']');
                i += 1;
            }
        } else {
            try result.append(allocator, iterable[i]);
            i += 1;
        }
    }

    return try allocator.dupe(u8, result.items);
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

            // Emit destructuring pattern declaration with initializer
            // TypeScript requires destructuring declarations to have an initializer
            try output.appendSlice(allocator, "var ");
            try output.appendSlice(allocator, pattern);
            try output.appendSlice(allocator, ": ");
            if (type_annotation) |t| {
                try output.appendSlice(allocator, t);
                try output.appendSlice(allocator, " = {} as ");
                try output.appendSlice(allocator, t);
            } else {
                try output.appendSlice(allocator, "any = {} as any");
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
                    // Stop at comma or standalone = (but not => for arrow function types)
                    if (depth == 0 and c == ',') break;
                    if (depth == 0 and c == '=' and (i + 1 >= params.len or params[i + 1] != '>')) break;
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
            // Must have initializer to avoid "used before being assigned" errors
            try output.appendSlice(allocator, "var ");
            try output.appendSlice(allocator, param_name);
            try output.appendSlice(allocator, ": ");
            if (type_annotation) |t| {
                try output.appendSlice(allocator, t);
                if (is_optional) {
                    try output.appendSlice(allocator, " | undefined");
                }
                try output.appendSlice(allocator, " = undefined as any");
            } else if (is_optional) {
                try output.appendSlice(allocator, "any | undefined = undefined");
            } else {
                try output.appendSlice(allocator, "any = undefined as any");
            }
            try output.appendSlice(allocator, ";\n");
            // Emit void statement to mark the parameter as used.
            // Snippet params are used in template expressions which may not emit void
            // statements (e.g., when param name matches a Svelte keyword like "key").
            try output.appendSlice(allocator, "void ");
            try output.appendSlice(allocator, param_name);
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

/// Extracts iterable and bindings from {#each items as item, i (key)}
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

    // Extract key expression if present: (key) after bindings
    var key_expr: ?[]const u8 = null;
    if (binding_end < content.len and content[binding_end] == '(') {
        // Find the matching closing paren
        var paren_depth: u32 = 1;
        const key_start = binding_end + 1;
        var key_end = key_start;
        while (key_end < content.len and paren_depth > 0) {
            if (content[key_end] == '(') paren_depth += 1;
            if (content[key_end] == ')') paren_depth -= 1;
            if (paren_depth > 0) key_end += 1;
        }
        const key = std.mem.trim(u8, content[key_start..key_end], " \t\n\r");
        if (key.len > 0) {
            key_expr = key;
        }
    }

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
        .key_expr = key_expr,
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

/// Extracts snippet name and params from {#snippet name(params)}
fn extractSnippetName(source: []const u8, start: u32, end: u32) ?SnippetNameInfo {
    const content = source[start..end];
    // Format: {#snippet name(params)}
    const prefix = "{#snippet ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const name_start = idx + prefix.len;

    // Find opening paren or closing brace (for snippets without params)
    var name_end = name_start;
    while (name_end < content.len) {
        const c = content[name_end];
        if (c == '(' or c == '}' or std.ascii.isWhitespace(c)) break;
        name_end += 1;
    }

    const name = std.mem.trim(u8, content[name_start..name_end], " \t\n\r");
    if (name.len == 0) return null;

    // Check for params
    var params: ?[]const u8 = null;
    if (name_end < content.len and content[name_end] == '(') {
        const params_start = name_end + 1;
        var paren_depth: u32 = 1;
        var i = params_start;
        while (i < content.len and paren_depth > 0) {
            if (content[i] == '(') paren_depth += 1;
            if (content[i] == ')') paren_depth -= 1;
            if (paren_depth > 0) i += 1;
        }
        const p = std.mem.trim(u8, content[params_start..i], " \t\n\r");
        if (p.len > 0) params = p;
    }

    return .{
        .name = name,
        .params = params,
        .offset = @intCast(name_start),
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
    // Early exit: skip if no Svelte imports to filter
    const has_svelte_import = std.mem.indexOf(u8, content, "from 'svelte'") != null or
        std.mem.indexOf(u8, content, "from \"svelte\"") != null;
    if (!has_svelte_import) return content;

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);
    try result.ensureTotalCapacity(allocator, content.len);

    var i: usize = 0;
    while (i < content.len) {
        const line_start = i;

        var line_end = i;
        while (line_end < content.len and content[line_end] != '\n') : (line_end += 1) {}

        const line = content[line_start..line_end];
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Check import source - we only filter types from 'svelte' (avoid duplicates)
        // ./$types imports are kept - they resolve via SvelteKit's generated types
        const is_svelte_import = std.mem.indexOf(u8, trimmed, "from 'svelte'") != null or
            std.mem.indexOf(u8, trimmed, "from \"svelte\"") != null;
        const should_filter = is_svelte_import;

        // We no longer filter type-only imports since we don't auto-import types.
        // User imports like `import type { Snippet } from 'svelte'` are preserved.
        // We only filter mixed imports to remove type specifiers that conflict with our stubs.
        if (should_filter and std.mem.startsWith(u8, trimmed, "import")) {
            // Type-only imports: keep them (e.g., `import type { Snippet } from 'svelte'`)
            if (std.mem.startsWith(u8, trimmed, "import type")) {
                try result.appendSlice(allocator, line);
            } else if (try filterMixedImport(allocator, trimmed)) |filtered_line| {
                // Mixed imports: remove type specifiers (e.g., `import { type X, tick } from ...`)
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

/// Transforms Svelte store auto-subscriptions ($storeName) to __svelte_store(storeName).v.
/// This allows tsgo to type-check store access patterns without seeing undefined variables.
/// The .v property allows both reads and assignments (for $store = value syntax).
/// Handles cases like:
/// - `$page.url` → `__svelte_store(page).v.url`
/// - `$count + 1` → `__svelte_store(count).v + 1`
/// - `if ($loading)` → `if (__svelte_store(loading).v)`
/// - `$count = 5` → `__svelte_store(count).v = 5`
/// Does NOT transform:
/// - Runes like `$state`, `$derived`, `$effect`, `$props`, `$bindable`, `$inspect`, `$host`
/// - Template strings like `${expr}`
/// Collects unique store names from script content (stores are $name where name is not a rune).
/// Returns a list of store names (without the $ prefix).
fn collectStoreNames(allocator: std.mem.Allocator, content: []const u8) ![]const []const u8 {
    var store_names: std.StringHashMapUnmanaged(void) = .empty;
    defer store_names.deinit(allocator);

    const runes = [_][]const u8{
        "state", "derived", "effect", "props", "bindable", "inspect", "host",
    };

    var i: usize = 0;
    while (i < content.len) {
        // Skip strings
        if (content[i] == '"' or content[i] == '\'' or content[i] == '`') {
            const quote = content[i];
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
                // For template literals, skip ${...} but scan inside for stores
                if (quote == '`' and content[i] == '$' and i + 1 < content.len and content[i + 1] == '{') {
                    i += 2;
                    var depth: u32 = 1;
                    while (i < content.len and depth > 0) {
                        if (content[i] == '{') depth += 1;
                        if (content[i] == '}') depth -= 1;
                        if (depth > 0) {
                            // Check for $store inside interpolation
                            if (content[i] == '$' and i + 1 < content.len and isIdentStartChar(content[i + 1])) {
                                const name_start = i + 1;
                                var name_end = name_start;
                                while (name_end < content.len and isIdentChar(content[name_end])) : (name_end += 1) {}
                                const name = content[name_start..name_end];

                                const is_rune = for (runes) |rune| {
                                    if (std.mem.eql(u8, name, rune)) break true;
                                } else false;

                                if (!is_rune and name.len > 0) {
                                    try store_names.put(allocator, name, {});
                                }
                                i = name_end;
                                continue;
                            }
                            i += 1;
                        }
                    }
                    if (i < content.len and content[i] == '}') i += 1;
                    continue;
                }
                i += 1;
            }
            continue;
        }

        // Skip single-line comments
        if (i + 1 < content.len and content[i] == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') i += 1;
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

        // Check for $ followed by identifier
        if (content[i] == '$') {
            // Skip template literal ${
            if (i + 1 < content.len and content[i + 1] == '{') {
                i += 1;
                continue;
            }

            if (i + 1 < content.len and isIdentStartChar(content[i + 1])) {
                const name_start = i + 1;
                var name_end = name_start;
                while (name_end < content.len and isIdentChar(content[name_end])) : (name_end += 1) {}
                const name = content[name_start..name_end];

                const is_rune = for (runes) |rune| {
                    if (std.mem.eql(u8, name, rune)) break true;
                } else false;

                if (!is_rune and name.len > 0) {
                    try store_names.put(allocator, name, {});
                }
                i = name_end;
                continue;
            }
        }

        i += 1;
    }

    // Convert to owned slice
    var result: std.ArrayList([]const u8) = .empty;
    var iter = store_names.keyIterator();
    while (iter.next()) |key| {
        try result.append(allocator, key.*);
    }
    return try result.toOwnedSlice(allocator);
}

/// Transforms store subscriptions by generating variable declarations.
/// For each unique store usage like $serverStatus, generates:
///   let $serverStatus = __svelte_store_get(serverStatus);
/// at the END of the content (after user declarations). The $storeName references
/// are kept as-is and refer to the declared variable, enabling TypeScript narrowing.
fn transformStoreSubscriptions(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Early exit: skip if no $ character (no stores or runes to process)
    if (std.mem.indexOfScalar(u8, content, '$') == null) return content;

    // Collect all unique store names
    const store_names = try collectStoreNames(allocator, content);
    defer allocator.free(store_names);

    // If no stores found, return content as-is
    if (store_names.len == 0) return content;

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);
    // Estimate: content + declarations (about 60 chars per store)
    try result.ensureTotalCapacity(allocator, content.len + store_names.len * 60);

    // Append original content first
    try result.appendSlice(allocator, content);

    // Generate store variable declarations at the end (after user declarations)
    // Use 'var' instead of 'let' for hoisting - this allows using $storeName
    // before the declaration line (var is hoisted to function/module scope)
    try result.appendSlice(allocator, "\n// Store subscriptions\n");
    for (store_names) |name| {
        try result.appendSlice(allocator, "var $");
        try result.appendSlice(allocator, name);
        try result.appendSlice(allocator, " = __svelte_store_get(");
        try result.appendSlice(allocator, name);
        try result.appendSlice(allocator, ");\n");
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
    // Early exit: skip if no "$:" pattern (no reactive statements)
    if (std.mem.indexOf(u8, content, "$:") == null) return content;

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);
    try result.ensureTotalCapacity(allocator, content.len);

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
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__SvelteComponentType__<$$Props") != null);
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

test "module script exports type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script module lang="ts">
        \\  export type MyVariant = 'primary' | 'secondary';
        \\  export const myVariants = ['primary', 'secondary'] as const;
        \\</script>
        \\<script lang="ts">
        \\  let { variant = 'primary' }: { variant: MyVariant } = $props();
        \\</script>
        \\<button>{variant}</button>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Module script exports should be visible at file top level
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type MyVariant") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export const myVariants") != null);
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

test "extract instance exports" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const content =
        \\  function handleKeyNavigation(event: KeyboardEvent) {
        \\    // do something
        \\  }
        \\  function scrollToBottom(behavior: 'smooth' | 'instant' = 'instant') {
        \\    // do something
        \\  }
        \\  export { handleKeyNavigation }
        \\  export function isReady(): boolean {
        \\    return true;
        \\  }
        \\  export const MAX_ITEMS = 10;
    ;

    var exports: std.ArrayList(ExportInfo) = .empty;
    try extractInstanceExports(allocator, content, &exports);

    // Should find 3 exports: handleKeyNavigation, isReady, MAX_ITEMS
    try std.testing.expectEqual(@as(usize, 3), exports.items.len);

    // handleKeyNavigation from export { ... } - type is null (looked up from definition returns null)
    try std.testing.expectEqualStrings("handleKeyNavigation", exports.items[0].name);
    try std.testing.expect(exports.items[0].type_repr == null);

    // isReady from export function - type is typeof isReady
    try std.testing.expectEqualStrings("isReady", exports.items[1].name);
    try std.testing.expectEqualStrings("typeof isReady", exports.items[1].type_repr.?);

    // MAX_ITEMS from export const - no explicit type
    try std.testing.expectEqualStrings("MAX_ITEMS", exports.items[2].name);
    try std.testing.expect(exports.items[2].type_repr == null);
}

test "instance exports in generated component type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  function handleKey(e: KeyboardEvent) {}
        \\  export { handleKey }
        \\  export function focus() { }
        \\</script>
    ;

    var parser: @import("svelte_parser.zig").Parser = .init(allocator, source, "test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // Should have $$Exports interface with the exports
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Exports") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "handleKey:") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "focus: typeof focus") != null);

    // $$Exports must have index signature for Record<string, any> compatibility (required by mount())
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "[key: string]: any;") != null);

    // Component type should use $$Exports
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__SvelteComponentType__<$$Props, $$Exports, $$Bindings>") != null);
}

test "$$Exports is compatible with Record<string, any> for mount()" {
    // Regression test: mount() returns Exports which must extend Record<string, any>.
    // Without an index signature, empty $$Exports {} is not assignable to Record<string, unknown>.
    // This caused false positives like: "Type '$$Exports' is not assignable to 'Record<string, unknown>'"
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Component with no exports - the common case that triggered the bug
    const source =
        \\<script lang="ts">
        \\  let { name }: { name: string } = $props();
        \\</script>
        \\<p>{name}</p>
    ;

    var parser: @import("svelte_parser.zig").Parser = .init(allocator, source, "test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // Even with no exports, $$Exports must have index signature
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Exports") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "[key: string]: any;") != null);
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
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Component as __SvelteComponentType__ }") != null);
    // Snippet import is conditional - only added when file uses snippets
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet }") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Props") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "name: string") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "count?: number") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export interface $$Slots") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "default: {}") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__SvelteComponentType__<$$Props, $$Exports, $$Bindings>") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "badType: number = \"not a number\"") != null);

    // Component usages in template should emit void statements
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Button;") != null);
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

test "extract $props() with multi-line destructuring" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Pattern from animated-text.svelte - destructuring spans multiple lines
    const content =
        \\let {
        \\    text,
        \\    startDelay = 0,
        \\    fadeOutDelay = undefined,
        \\    kerningAdjustments = {},
        \\    class: classList = '',
        \\} = $props()
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    const interface_name = try extractPropsRune(allocator, content, &props);

    try std.testing.expect(interface_name == null);
    try std.testing.expectEqual(@as(usize, 5), props.items.len);
    try std.testing.expectEqualStrings("text", props.items[0].name);
    try std.testing.expect(!props.items[0].has_initializer);
    try std.testing.expectEqualStrings("startDelay", props.items[1].name);
    try std.testing.expect(props.items[1].has_initializer);
    try std.testing.expectEqualStrings("fadeOutDelay", props.items[2].name);
    try std.testing.expect(props.items[2].has_initializer);
    try std.testing.expectEqualStrings("kerningAdjustments", props.items[3].name);
    try std.testing.expect(props.items[3].has_initializer);
    // Renamed prop "class: classList" should extract "class" as the prop name
    try std.testing.expectEqualStrings("class", props.items[4].name);
    try std.testing.expect(props.items[4].has_initializer);
}

test "extract $props() with complex multi-line intersection type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Pattern from action-row.svelte - Pick<...> & {...} with arrow functions
    const content =
        \\let {
        \\    icon,
        \\    isExpanded = true,
        \\    hintOnClick = undefined,
        \\}: Pick<
        \\    ComponentProps,
        \\    | 'icon'
        \\    | 'isExpanded'
        \\> & {
        \\    children: Snippet
        \\    hintOnClick?: () => void
        \\} = $props()
    ;

    var props: std.ArrayList(PropInfo) = .empty;
    const interface_name = try extractPropsRune(allocator, content, &props);

    // Should extract the full complex type including Pick and intersection
    try std.testing.expect(interface_name != null);
    try std.testing.expect(std.mem.indexOf(u8, interface_name.?, "Pick<") != null);
    try std.testing.expect(std.mem.indexOf(u8, interface_name.?, "& {") != null);
    try std.testing.expect(std.mem.indexOf(u8, interface_name.?, "() => void") != null);

    // Should extract the props from destructuring
    try std.testing.expectEqual(@as(usize, 3), props.items.len);
    try std.testing.expectEqualStrings("icon", props.items[0].name);
    try std.testing.expect(!props.items[0].has_initializer);
    try std.testing.expectEqualStrings("isExpanded", props.items[1].name);
    try std.testing.expect(props.items[1].has_initializer);
    try std.testing.expectEqualStrings("hintOnClick", props.items[2].name);
    try std.testing.expect(props.items[2].has_initializer);
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

    // We don't auto-import Snippet - user's import is preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet } from 'svelte'") != null);
    // Snippet type annotation from user code is preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "children?: Snippet") != null);

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

// ============================================================================
// Svelte 5 rune type declaration tests
// ============================================================================

test "rune stubs: $state with type inference" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  let name = $state<string>('hello');
        \\  let items = $state<string[]>([]);
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "State.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $state declaration exists with correct signature
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $state<T>(initial: T): T;") != null);
    // Verify script content is preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let count = $state(0);") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let name = $state<string>('hello');") != null);
}

test "rune stubs: $state.raw and $state.snapshot" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let data = $state.raw({ x: 1, y: 2 });
        \\  function save() {
        \\    const snapshot = $state.snapshot(data);
        \\    console.log(snapshot);
        \\  }
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "StateRaw.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $state namespace with raw and snapshot
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function raw<T>(initial: T): T;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function snapshot<T>(state: T): T;") != null);
    // Verify script content
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$state.raw({ x: 1, y: 2 })") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$state.snapshot(data)") != null);
}

test "rune stubs: $derived with expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  let doubled = $derived(count * 2);
        \\  let message = $derived(`Count is ${count}`);
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Derived.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $derived declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $derived<T>(expr: T): T;") != null);
    // Verify usage is preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$derived(count * 2)") != null);
}

test "rune stubs: $derived.by with function" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items = $state<number[]>([1, 2, 3]);
        \\  let total = $derived.by(() => {
        \\    return items.reduce((a, b) => a + b, 0);
        \\  });
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "DerivedBy.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $derived namespace with by
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $derived") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function by<T>(fn: () => T): T;") != null);
    // Verify usage
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$derived.by(()") != null);
}

test "rune stubs: $effect basic usage" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  $effect(() => {
        \\    console.log('count changed:', count);
        \\  });
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Effect.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $effect declaration with cleanup return type
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $effect(fn: () => void | (() => void)): void;") != null);
}

test "rune stubs: $effect.pre and $effect.root" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  
        \\  $effect.pre(() => {
        \\    console.log('before DOM update');
        \\  });
        \\  
        \\  const cleanup = $effect.root(() => {
        \\    console.log('root effect');
        \\    return () => console.log('cleanup');
        \\  });
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "EffectPre.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $effect namespace functions
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $effect") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function pre(fn: () => void | (() => void)): void;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function root(fn: () => void | (() => void)): () => void;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function tracking(): boolean;") != null);
    // Verify usage
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$effect.pre(()") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$effect.root(()") != null);
}

test "rune stubs: $props basic usage" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let { name, age = 0 } = $props();
        \\</script>
        \\<p>{name} is {age} years old</p>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Props.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $props declaration with default generic
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $props<T = $$Props>(): T;") != null);
}

test "rune stubs: $props with typed interface" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  interface ButtonProps {
        \\    label: string;
        \\    disabled?: boolean;
        \\    onclick?: () => void;
        \\  }
        \\  let { label, disabled = false, onclick }: ButtonProps = $props();
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Button.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify the interface is used for $$Props
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type $$Props = ButtonProps;") != null);
}

test "rune stubs: $bindable for two-way binding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let { value = $bindable('') } = $props();
        \\</script>
        \\<input bind:value />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Input.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $bindable declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $bindable<T>(initial?: T): T;") != null);
    // Verify usage is preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$bindable('')") != null);
}

test "rune stubs: $inspect for debugging" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  $inspect(count);
        \\  $inspect(count).with(console.log);
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Inspect.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $inspect declaration with .with() chaining
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $inspect<T>(...values: T[]): { with: (fn: (type: 'init' | 'update', ...values: T[]) => void) => void };") != null);
    // Verify usage
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$inspect(count)") != null);
}

test "rune stubs: $host for custom elements" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  const host = $host<HTMLDivElement>();
        \\  $effect(() => {
        \\    host.style.color = 'red';
        \\  });
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "CustomElement.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify $host declaration with HTMLElement constraint
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $host<T extends HTMLElement>(): T;") != null);
    // Verify usage
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$host<HTMLDivElement>()") != null);
}

test "rune stubs: all runes in single component" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  interface Props { initial?: number }
        \\  
        \\  let { initial = 0 }: Props = $props();
        \\  let count = $state(initial);
        \\  let doubled = $derived(count * 2);
        \\  let expensive = $derived.by(() => count ** 10);
        \\  
        \\  $effect(() => {
        \\    console.log(count);
        \\  });
        \\  
        \\  $effect.pre(() => {
        \\    // before DOM
        \\  });
        \\  
        \\  $inspect(count, doubled);
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "AllRunes.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Verify all main rune declarations are present
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $derived") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $effect") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $props") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $bindable") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $inspect") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $host") != null);

    // Verify namespace declarations
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $state") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $derived") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare namespace $effect") != null);
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

    // User's ./$types import should be preserved (not filtered, not auto-generated)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { PageData } from './$types';") != null);
    // Should still have Svelte Component import
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__SvelteComponentType__") != null);
}

test "transform sveltekit +layout.svelte route" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import type { LayoutData } from './$types';
        \\  import type { Snippet } from 'svelte';
        \\  let { children, data }: { children: Snippet; data: LayoutData } = $props();
        \\</script>
        \\{@render children()}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/+layout.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // User's ./$types import should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { LayoutData } from './$types';") != null);
}

test "transform sveltekit +page@.svelte route variant" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import type { PageData } from './$types';
        \\  let { data }: { data: PageData } = $props();
        \\</script>
        \\<p>{data.count}</p>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "src/routes/admin/+page@.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // User's ./$types import should be preserved for route variants too
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { PageData } from './$types';") != null);
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

    // Content is preserved, with var declaration appended at end
    try std.testing.expect(std.mem.startsWith(u8, result, "const url = $page.url;"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $page = __svelte_store_get(page);") != null);
}

test "transform store subscription preserves runes" {
    const allocator = std.testing.allocator;

    const input = "let count = $state(0); let doubled = $derived(count * 2);";
    const result = try transformStoreSubscriptions(allocator, input);

    // Runes like $state and $derived are preserved as-is (no store declarations generated)
    try std.testing.expectEqualStrings("let count = $state(0); let doubled = $derived(count * 2);", result);
}

test "transform store subscription multiple stores" {
    const allocator = std.testing.allocator;

    const input = "if ($page.url && $navigating) { console.log($myStore); }";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    // Content is preserved, with var declarations appended at end for each store
    try std.testing.expect(std.mem.startsWith(u8, result, "if ($page.url && $navigating) { console.log($myStore); }"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $page = __svelte_store_get(page);") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "var $navigating = __svelte_store_get(navigating);") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "var $myStore = __svelte_store_get(myStore);") != null);
}

test "transform store subscription skips template literal interpolation marker" {
    const allocator = std.testing.allocator;

    const input = "const msg = `url: ${$page.url}`;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    // Content is preserved with $page kept in template literal, declaration appended
    try std.testing.expect(std.mem.startsWith(u8, result, "const msg = `url: ${$page.url}`;"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $page = __svelte_store_get(page);") != null);
}

test "transform store subscription skips strings" {
    const allocator = std.testing.allocator;

    const input = "const str = '$page is a store'; const real = $page;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    // Content is preserved; $page inside string is NOT collected (only real code usage)
    try std.testing.expect(std.mem.startsWith(u8, result, "const str = '$page is a store'; const real = $page;"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $page = __svelte_store_get(page);") != null);
}

test "transform store subscription mixed runes and stores" {
    const allocator = std.testing.allocator;

    const input = "let url = $state($page.url);";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    // Content is preserved; $state is a rune (not collected), $page is a store
    try std.testing.expect(std.mem.startsWith(u8, result, "let url = $state($page.url);"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $page = __svelte_store_get(page);") != null);
    // Should NOT generate declaration for runes
    try std.testing.expect(std.mem.indexOf(u8, result, "var $state") == null);
}

test "transform store subscription assignment" {
    const allocator = std.testing.allocator;

    const input = "$count = 5;";
    const result = try transformStoreSubscriptions(allocator, input);
    defer allocator.free(result);

    // Store assignment preserved as-is (becomes variable assignment after hoisting)
    try std.testing.expect(std.mem.startsWith(u8, result, "$count = 5;"));
    try std.testing.expect(std.mem.indexOf(u8, result, "var $count = __svelte_store_get(count);") != null);
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

    // Should have $page.url preserved in content
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$page.url") != null);
    // Should have var declaration for $page using __svelte_store_get
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var $page = __svelte_store_get(page);") != null);
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

    // Each blocks are no longer emitted to avoid scope issues with nested blocks
    // Just verify component typing is present
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "// Component typing") != null);
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

    // Should have param declarations with types and initializers
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name: string = undefined as any;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var age: number = undefined as any;") != null);
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

    // We don't auto-import Snippet (users import it themselves if needed)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet }") == null);
    // Should handle import() types with parentheses correctly (with initializers)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var key: import('svelte').Snippet | string = undefined as any;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var label: string = undefined as any;") != null);
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

    // Template expressions (each blocks, const tags) are no longer emitted at top-level
    // to avoid scope issues with block-scoped bindings
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "// Component typing") != null);
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

    // Should generate valid TypeScript with | undefined and initializer, not ?
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name: string | undefined = undefined as any;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var name?: string;") == null);
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

    // Should handle object destructuring as a single pattern with initializer
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var { wrapperProps, props, open }: any = {} as any;") != null);
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

    // Template expressions (each, const) are no longer emitted to avoid scope issues
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "// Component typing") != null);
}

test "emit generic type declarations - single" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "T");

    try std.testing.expectEqualStrings("type T = unknown;\n", output.items);
}

test "emit generic type declarations - with extends" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "T extends SomeType");

    try std.testing.expectEqualStrings("type T = SomeType;\n", output.items);
}

test "emit generic type declarations - multiple" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "T, U extends Bar");

    try std.testing.expectEqualStrings("type T = unknown;\ntype U = Bar;\n", output.items);
}

test "emit generic type declarations - complex constraint" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "T extends Record<string, number>");

    try std.testing.expectEqualStrings("type T = Record<string, number>;\n", output.items);
}

test "emit generic type declarations - with default type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "T extends SomeType = SomeType");

    try std.testing.expectEqualStrings("type T = SomeType;\n", output.items);
}

test "emit generic type declarations - default type with complex generics" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output: std.ArrayList(u8) = .empty;
    try emitGenericTypeDeclarations(allocator, &output, "ToolDef extends ToolDefinition = ToolDefinition");

    try std.testing.expectEqualStrings("type ToolDef = ToolDefinition;\n", output.items);
}

test "transform with generics attribute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts" generics="T extends SomeInterface">
        \\    let value: T;
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Generic.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have generic type declaration
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "type T = SomeInterface;") != null);
}

test "transform with module and instance generics" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts" module>
        \\    export type ComboboxItem<T> = { id: string; value: T; label: string }
        \\</script>
        \\
        \\<script lang="ts" generics="T">
        \\    let items: ComboboxItem<T>[] = [];
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Combobox.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should have generic type declaration for T
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "type T = unknown;") != null);
    // Should also have the module script content
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type ComboboxItem<T>") != null);
}

test "transform with renamed $props has only one default export" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\import * as Tooltip from '@sourcegraph/amp-web-ui/components/tooltip'
        \\let {
        \\        component: Component,
        \\        props = {},
        \\}: {
        \\        component: any
        \\        props?: Record<string, any>
        \\} = $props()
        \\</script>
        \\<Tooltip.Provider>
        \\<Component {...props} />
        \\</Tooltip.Provider>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "thread-expansion-test-wrapper.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Count export default occurrences - should be exactly 1
    var count: usize = 0;
    var i: usize = 0;
    while (std.mem.indexOfPos(u8, virtual.content, i, "export default")) |pos| {
        count += 1;
        i = pos + 1;
    }
    if (count != 1) {
        std.debug.print("\n=== Generated virtual file ===\n{s}\n=== End ===\n", .{virtual.content});
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

// ============================================================================
// Event Handler Tests
// ============================================================================

test "event handler: on:click directive with function reference" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\  
        \\  function handleClick(event: MouseEvent) {
        \\    count++;
        \\  }
        \\</script>
        \\
        \\<button on:click={handleClick}>Count: {count}</button>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Button.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Event handler function should be preserved in script
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleClick(event: MouseEvent)") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "count++") != null);
    // Component export should be generated
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__SvelteComponent__: __SvelteComponentType__<$$Props") != null);
}

test "event handler: on:click with inline arrow function" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\</script>
        \\
        \\<button on:click={() => count++}>Increment</button>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Counter.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Script content should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let count = $state(0)") != null);
    // $state rune stub should be declared
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $state<T>(initial: T): T;") != null);
}

test "event handler: on:change for input" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let value = $state("");
        \\  
        \\  function handleChange(event: Event) {
        \\    const target = event.target as HTMLInputElement;
        \\    value = target.value;
        \\  }
        \\</script>
        \\
        \\<input on:change={handleChange} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Input.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Handler function and type assertion should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleChange(event: Event)") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "event.target as HTMLInputElement") != null);
}

test "event handler: multiple events on same element" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  function handleMouseEnter() { console.log('enter'); }
        \\  function handleMouseLeave() { console.log('leave'); }
        \\  function handleClick() { console.log('click'); }
        \\</script>
        \\
        \\<div on:mouseenter={handleMouseEnter} on:mouseleave={handleMouseLeave} on:click={handleClick}>
        \\  Hover me
        \\</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Hover.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // All handlers should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleMouseEnter()") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleMouseLeave()") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleClick()") != null);
}

test "event handler: Svelte 5 onclick attribute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let count = $state(0);
        \\</script>
        \\
        \\<button onclick={() => count++}>Svelte 5 style</button>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Button5.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Svelte 5 uses regular onclick attributes
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let count = $state(0)") != null);
}

test "event handler: with event modifiers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  function handleSubmit(event: Event) {
        \\    // Form submitted
        \\  }
        \\</script>
        \\
        \\<form on:submit|preventDefault={handleSubmit}>
        \\  <button type="submit">Submit</button>
        \\</form>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Form.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Handler should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleSubmit(event: Event)") != null);
}

// ============================================================================
// Binding Tests
// ============================================================================

test "binding: bind:value on text input" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let name = $state("");
        \\</script>
        \\
        \\<input type="text" bind:value={name} placeholder="Enter name" />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "NameInput.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // State should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let name = $state(\"\")") != null);
}

test "binding: bind:value shorthand" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let value = $state("");
        \\</script>
        \\
        \\<input bind:value />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Shorthand.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Shorthand binding should work
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let value = $state(\"\")") != null);
}

test "binding: bind:checked on checkbox" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let accepted = $state(false);
        \\</script>
        \\
        \\<label>
        \\  <input type="checkbox" bind:checked={accepted} />
        \\  Accept terms
        \\</label>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Checkbox.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Boolean state should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let accepted = $state(false)") != null);
}

test "binding: bind:group for radio buttons" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let selected = $state<string | null>(null);
        \\</script>
        \\
        \\<label>
        \\  <input type="radio" bind:group={selected} value="a" />
        \\  Option A
        \\</label>
        \\<label>
        \\  <input type="radio" bind:group={selected} value="b" />
        \\  Option B
        \\</label>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "RadioGroup.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Generic state type should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let selected = $state<string | null>(null)") != null);
}

test "binding: bind:group for checkboxes (array)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let flavors = $state<string[]>([]);
        \\</script>
        \\
        \\<label>
        \\  <input type="checkbox" bind:group={flavors} value="chocolate" />
        \\  Chocolate
        \\</label>
        \\<label>
        \\  <input type="checkbox" bind:group={flavors} value="vanilla" />
        \\  Vanilla
        \\</label>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Flavors.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Array state type should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let flavors = $state<string[]>([])") != null);
}

test "binding: bind:this for element reference" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let canvas: HTMLCanvasElement;
        \\  
        \\  $effect(() => {
        \\    const ctx = canvas?.getContext('2d');
        \\    if (ctx) {
        \\      ctx.fillRect(0, 0, 100, 100);
        \\    }
        \\  });
        \\</script>
        \\
        \\<canvas bind:this={canvas}></canvas>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Canvas.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Element reference type should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let canvas: HTMLCanvasElement") != null);
    // Effect using the reference should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "canvas?.getContext('2d')") != null);
}

test "binding: multiple bindings on same element" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let value = $state("");
        \\  let inputEl: HTMLInputElement;
        \\</script>
        \\
        \\<input bind:value={value} bind:this={inputEl} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "MultiBinding.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Both bindings should be supported
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let value = $state(\"\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let inputEl: HTMLInputElement") != null);
}

// ============================================================================
// Two-way Binding with $bindable Tests
// ============================================================================

test "two-way binding: $bindable prop with bind:value" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let { value = $bindable("") } = $props<{ value?: string }>();
        \\</script>
        \\
        \\<input bind:value={value} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "BindableInput.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // $bindable declaration and usage should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "declare function $bindable<T>(initial?: T): T;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$bindable(\"\")") != null);
}

test "two-way binding: component with bindable checked" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  interface Props {
        \\    checked?: boolean;
        \\  }
        \\  
        \\  let { checked = $bindable(false) }: Props = $props();
        \\</script>
        \\
        \\<input type="checkbox" bind:checked={checked} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "BindableCheckbox.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Interface and bindable usage should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "interface Props") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "$bindable(false)") != null);
}

// ============================================================================
// Combined Event + Binding Tests
// ============================================================================

test "combined: input with both event and binding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let value = $state("");
        \\  
        \\  function handleKeydown(event: KeyboardEvent) {
        \\    if (event.key === 'Enter') {
        \\      console.log('Submitted:', value);
        \\    }
        \\  }
        \\</script>
        \\
        \\<input bind:value={value} on:keydown={handleKeydown} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "SearchInput.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Both state and handler should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let value = $state(\"\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleKeydown(event: KeyboardEvent)") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "event.key === 'Enter'") != null);
}

test "combined: form with submit handler and multiple bindings" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let email = $state("");
        \\  let password = $state("");
        \\  let rememberMe = $state(false);
        \\  
        \\  function handleSubmit(event: Event) {
        \\    event.preventDefault();
        \\    console.log({ email, password, rememberMe });
        \\  }
        \\</script>
        \\
        \\<form on:submit={handleSubmit}>
        \\  <input type="email" bind:value={email} />
        \\  <input type="password" bind:value={password} />
        \\  <input type="checkbox" bind:checked={rememberMe} />
        \\  <button type="submit">Login</button>
        \\</form>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "LoginForm.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // All state and handlers should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let email = $state(\"\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let password = $state(\"\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let rememberMe = $state(false)") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleSubmit(event: Event)") != null);
}

test "combined: select with bind:value and on:change" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  type Option = { value: string; label: string };
        \\
        \\  let selected = $state("");
        \\  let options: Option[] = [
        \\    { value: "a", label: "Option A" },
        \\    { value: "b", label: "Option B" },
        \\  ];
        \\
        \\  function handleChange() {
        \\    console.log('Selected:', selected);
        \\  }
        \\</script>
        \\
        \\<select bind:value={selected} on:change={handleChange}>
        \\  {#each options as opt}
        \\    <option value={opt.value}>{opt.label}</option>
        \\  {/each}
        \\</select>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Select.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Type, state, and handler should be preserved
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "type Option = { value: string; label: string }") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let selected = $state(\"\")") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function handleChange()") != null);
}

test "snippet name shadowing import should not emit var declaration" {
    // Regression test: when a snippet has the same name as an import (e.g., {#snippet filter()}
    // with `import { filter } from 'rxjs'`), we should NOT emit `var filter: any = null;`
    // because it would conflict with the import. In Svelte, snippets create a new scope
    // and can shadow imports, but in generated TS everything is at module scope.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import { filter, map } from 'rxjs';
        \\  import Default from './other';
        \\  import * as Namespace from './namespace';
        \\
        \\  let data = $state([]);
        \\</script>
        \\
        \\<div>
        \\  {#snippet filter()}
        \\    <span>Filter UI</span>
        \\  {/snippet}
        \\  {#snippet map()}
        \\    <span>Map UI</span>
        \\  {/snippet}
        \\  {#snippet Default()}
        \\    <span>Default UI</span>
        \\  {/snippet}
        \\  {#snippet other()}
        \\    <span>Other UI (not imported)</span>
        \\  {/snippet}
        \\</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "SnippetShadow.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Should NOT emit var declarations for snippet names that shadow imports
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var filter: any = null;") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var map: any = null;") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var Default: any = null;") == null);

    // SHOULD emit var declaration for snippet that doesn't shadow an import
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var other: any = null;") != null);

    // The imports should still be present
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import { filter, map } from 'rxjs'") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import Default from './other'") != null);
}

test "component usages in template emit void statements" {
    // Verify that components used in templates emit void statements to mark them as used.
    // This prevents "X is declared but never used" errors for component imports.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import Button from './Button.svelte';
        \\  import UserCard from './UserCard.svelte';
        \\</script>
        \\
        \\<div>
        \\  <Button />
        \\  <UserCard name="test" />
        \\</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "ComponentTest.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Simple components should emit void statements
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Button;") != null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void UserCard;") != null);
}

test "component usages nested inside snippets emit void statements" {
    // Verify components used inside {#snippet} blocks are detected.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import Button from './Button.svelte';
        \\</script>
        \\<Popover.Trigger>
        \\  {#snippet child({ props })}
        \\    <Button variant="outline" {...props}>Click</Button>
        \\  {/snippet}
        \\</Popover.Trigger>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Components inside snippets should emit void statements
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Button;") != null);
    // Popover should also be marked as used
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Popover;") != null);
    // Spread props inside attributes should be detected
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void props;") != null);
}

test "spread props in lowercase tags are detected" {
    // Verify that spread props like {...props} in lowercase tags are detected.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let { children, ...props } = $props();
        \\</script>
        \\<a class="link" {...props}>Click</a>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Spread props in lowercase tags should emit void statements
    const found = std.mem.indexOf(u8, virtual.content, "void props;") != null;
    if (!found) {
        std.debug.print("\n=== GENERATED OUTPUT ===\n{s}\n=== END ===\n", .{virtual.content});
    }
    try std.testing.expect(found);
}

test "each block with nullable iterable emits null-safe fallback" {
    // Svelte tolerates null/undefined iterables in {#each} (treats them as empty).
    // The generated TS must use ?? [] fallback to avoid "Type 'null' must have
    // a '[Symbol.iterator]()' method" errors.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items: string[] | null = $state(null);
        \\</script>
        \\{#each items as item}
        \\  <p>{item}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The generated binding should use ?? [] fallback for null safety
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "((items) ?? [])[0]") != null);
}

test "each block with spread of nullable array emits null-safe fallback" {
    // When spreading a nullable array like [...items], we need to make the spread null-safe.
    // This handles patterns like: {#each showAbove ? [...items].reverse() : items as item}
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let items: string[] | null = $state(null);
        \\  let reverse = $state(false);
        \\</script>
        \\{#each reverse ? [...items].reverse() : items as item}
        \\  <p>{item}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The spread should be wrapped with ?? [] for null safety: [...(items ?? [])]
    const found = std.mem.indexOf(u8, virtual.content, "[...(items ?? [])]") != null;
    if (!found) {
        std.debug.print("\n=== GENERATED OUTPUT ===\n{s}\n=== END ===\n", .{virtual.content});
    }
    try std.testing.expect(found);
}

test "identifier in attribute expression is marked as used" {
    // Test that an imported value used in an attribute expression like icon={Map}
    // is properly detected and emits a void statement.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  import Map from './map';
        \\</script>
        \\<Icon icon={Map} />
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // "Map" should appear as void statement since it's used in template
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Map;") != null);
    // "Icon" should also be marked as used
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Icon;") != null);
}

test "braces inside string literals in attribute expressions are not counted" {
    // Regression test: When scanning attribute expressions like {...toolCallProps({...})},
    // braces inside string literals (e.g., '{ foo }') must not affect brace depth counting.
    // This was causing spurious identifier extraction from HTML text content.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // The expression contains a string with unbalanced-looking braces: '{ foo'
    // If we incorrectly count these braces, we'd exit the expression early and
    // treat subsequent text as identifiers.
    const source =
        \\<script lang="ts">
        \\  function getProps() { return { str: '{ unclosed' }; }
        \\</script>
        \\<Component {...getProps()} />
        \\<h4>Diff text should not be extracted</h4>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // "Diff" from the <h4> text should NOT appear as a void statement
    // (it's plain text content, not an identifier reference)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void Diff;") == null);

    // "getProps" should appear since it's used in the expression
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void getProps;") != null);
}

test "keyed each with destructured key emits void for key expression" {
    // When a keyed {#each} uses a destructured variable as the key expression,
    // that variable should be marked as used. Example:
    // {#each Object.entries(map) as [key, value] (key)} - key is only used in (key)
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let map: Record<string, number> = {};
        \\</script>
        \\{#each Object.entries(map) as [key, value] (key)}
        \\  <p>{value}</p>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The key expression should be emitted as void (key)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void (key);") != null);
}
