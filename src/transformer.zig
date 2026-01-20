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
const AttributeData = @import("svelte_parser.zig").AttributeData;
const SourceMap = @import("source_map.zig").SourceMap;
const sveltekit = @import("sveltekit.zig");
const Diagnostic = @import("diagnostic.zig").Diagnostic;

pub const VirtualFile = struct {
    original_path: []const u8,
    virtual_path: []const u8,
    content: []const u8,
    source_map: SourceMap,
    is_typescript: bool, // true if script has lang="ts"
    /// Diagnostic for TS syntax in JS scripts (e.g., "Unexpected token" at first type annotation)
    ts_syntax_diagnostic: ?Diagnostic = null,
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

/// Tracks a snippet's body range for scoping purposes.
/// Expressions inside a snippet body use snippet parameters and should be
/// emitted inside the snippet function, not at module scope.
const SnippetBodyRange = struct {
    name: []const u8,
    params: ?[]const u8,
    generics: ?[]const u8,
    /// Position of the {#snippet tag (for finding enclosing if branches)
    snippet_start: u32,
    /// Start of snippet body (after the closing } of {#snippet ...})
    body_start: u32,
    /// End of snippet body (start of {/snippet})
    body_end: u32,
};

/// Represents a branch within an {#if}/{:else if}/{:else}/{/if} chain.
/// Each branch has a condition (or null for {:else}) and a body range.
/// Expressions in the body should be narrowed by the condition.
const IfBranch = struct {
    /// The condition expression (null for {:else} branches)
    condition: ?[]const u8,
    /// True if this is a negated branch (inside {:else})
    is_else: bool,
    /// Start of this branch's body (after the tag's closing })
    body_start: u32,
    /// End of this branch's body (start of {:else} or {/if})
    body_end: u32,
    /// Prior conditions from earlier branches in the same if/else chain.
    /// For {:else if} and {:else} branches, these conditions must have been false
    /// for execution to reach this branch. Used for discriminated union narrowing.
    prior_conditions: []const []const u8,
};

/// Tracks an {#each} block's body range and bindings.
/// Elements inside an {#each} block need access to the loop variable.
const EachBodyRange = struct {
    /// The iterable expression (e.g., "items" in {#each items as item})
    iterable: []const u8,
    /// The item binding pattern (e.g., "item" or "{a, b}")
    item_binding: []const u8,
    /// Optional index binding (e.g., "i" in {#each items as item, i})
    index_binding: ?[]const u8,
    /// Start of the each block's body (after the closing } of {#each ...})
    body_start: u32,
    /// End of the each block's body (start of {:else} or {/each})
    body_end: u32,
};

/// Tracks the currently open if-block for narrowing context.
/// When emitting multiple expressions from the same if-branch, we keep the if-block
/// open so TypeScript can connect the narrowing context across expressions.
const NarrowingContext = struct {
    /// The enclosing branches for the currently open if-block (sorted by body_start).
    /// Empty means no if-block is currently open.
    enclosing_branches: std.ArrayList(IfBranch),
    /// Number of if-blocks currently open (matches enclosing_branches.items.len when open).
    conditions_opened: u32,
    /// True if template expressions header has been emitted.
    has_expressions: bool,
    /// The allocator for managing enclosing_branches.
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) NarrowingContext {
        return .{
            .enclosing_branches = .empty,
            .conditions_opened = 0,
            .has_expressions = false,
            .allocator = allocator,
        };
    }

    fn deinit(self: *NarrowingContext) void {
        self.enclosing_branches.deinit(self.allocator);
    }

    /// Check if two branches match by their body_start and body_end (unique identifiers).
    fn branchesEqual(a: IfBranch, b: IfBranch) bool {
        return a.body_start == b.body_start and a.body_end == b.body_end;
    }

    /// Find how many leading branches match between current and new contexts.
    fn findCommonPrefixLen(current: []const IfBranch, new: []const IfBranch) usize {
        const min_len = @min(current.len, new.len);
        var i: usize = 0;
        while (i < min_len) : (i += 1) {
            if (!branchesEqual(current[i], new[i])) break;
        }
        return i;
    }

    /// Close the currently open if-block (if any).
    fn close(self: *NarrowingContext, output: *std.ArrayList(u8)) !void {
        var j: u32 = 0;
        while (j < self.conditions_opened) : (j += 1) {
            try output.appendSlice(self.allocator, "}\n");
        }
        self.conditions_opened = 0;
        self.enclosing_branches.clearRetainingCapacity();
    }

    /// Close some inner if-blocks, keeping the outer ones open.
    fn closeInner(self: *NarrowingContext, output: *std.ArrayList(u8), keep_count: usize) !void {
        const to_close = self.conditions_opened - @as(u32, @intCast(keep_count));
        var j: u32 = 0;
        while (j < to_close) : (j += 1) {
            try output.appendSlice(self.allocator, "}\n");
        }
        self.conditions_opened = @intCast(keep_count);
        self.enclosing_branches.shrinkRetainingCapacity(keep_count);
    }

    /// Ensure the if-block for the given branches is open.
    /// Handles incremental opening/closing to maintain nested contexts.
    fn ensureOpen(
        self: *NarrowingContext,
        output: *std.ArrayList(u8),
        new_enclosing: []const IfBranch,
    ) !void {
        // Emit template expressions header if needed
        if (!self.has_expressions) {
            try output.appendSlice(self.allocator, ";// Template expressions\n");
            self.has_expressions = true;
        }

        // Find how many leading branches are the same
        const common_prefix_len = findCommonPrefixLen(self.enclosing_branches.items, new_enclosing);

        // If there's a complete match, nothing to do
        if (common_prefix_len == self.enclosing_branches.items.len and common_prefix_len == new_enclosing.len) {
            return;
        }

        // Close branches that diverge from the new context
        if (common_prefix_len < self.enclosing_branches.items.len) {
            try self.closeInner(output, common_prefix_len);
        }

        // Open new branches that extend beyond the common prefix
        if (common_prefix_len < new_enclosing.len) {
            const new_branches = new_enclosing[common_prefix_len..];
            const additional = try emitIfConditionOpeners(self.allocator, output, new_branches);
            self.conditions_opened += additional;
            try self.enclosing_branches.appendSlice(self.allocator, new_branches);
        }
    }

    /// Emit an expression within the current narrowing context.
    /// The if-block is kept open across multiple expressions with the same enclosing branches.
    fn emitExpression(
        self: *NarrowingContext,
        output: *std.ArrayList(u8),
        mappings: *std.ArrayList(SourceMap.Mapping),
        expr: []const u8,
        svelte_offset: u32,
        if_branches: []const IfBranch,
    ) !void {
        // Find all enclosing if branches for this expression
        var enclosing = try findAllEnclosingIfBranches(self.allocator, svelte_offset, if_branches);
        defer enclosing.deinit(self.allocator);

        // Ensure we're in the right if-block context
        try self.ensureOpen(output, enclosing.items);

        // Emit the actual expression
        try output.appendSlice(self.allocator, "void (");
        try mappings.append(self.allocator, .{
            .svelte_offset = svelte_offset,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(expr.len),
        });
        try output.appendSlice(self.allocator, expr);
        try output.appendSlice(self.allocator, ");\n");
    }

    /// Emit {#each} binding declarations within the current narrowing context.
    fn emitEachBinding(
        self: *NarrowingContext,
        output: *std.ArrayList(u8),
        mappings: *std.ArrayList(SourceMap.Mapping),
        binding: EachBindingInfo,
        is_typescript: bool,
        if_branches: []const IfBranch,
    ) !void {
        // Find all enclosing if branches for this {#each} block
        var enclosing = try findAllEnclosingIfBranches(self.allocator, binding.offset, if_branches);
        defer enclosing.deinit(self.allocator);

        // Ensure we're in the right if-block context
        try self.ensureOpen(output, enclosing.items);

        // Emit the binding declarations
        try emitEachBindingDeclarations(self.allocator, output, mappings, binding, binding.iterable, is_typescript);
    }

    /// Emit {@const} binding within the current narrowing context.
    fn emitConstBinding(
        self: *NarrowingContext,
        output: *std.ArrayList(u8),
        binding: ConstBindingInfo,
        svelte_offset: u32,
        if_branches: []const IfBranch,
    ) !void {
        // Find all enclosing if branches for this {@const}
        var enclosing = try findAllEnclosingIfBranches(self.allocator, svelte_offset, if_branches);
        defer enclosing.deinit(self.allocator);

        // Ensure we're in the right if-block context
        try self.ensureOpen(output, enclosing.items);

        // Emit the const binding
        try output.appendSlice(self.allocator, "var ");
        try output.appendSlice(self.allocator, binding.name);
        try output.appendSlice(self.allocator, " = ");
        try output.appendSlice(self.allocator, binding.expr);
        try output.appendSlice(self.allocator, ";\n");
    }
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

    // Separate module and instance scripts first (need this for is_typescript check)
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

    // Check if any script has lang="ts" OR generics (TypeScript mode)
    // The generics attribute implies TypeScript mode even without lang="ts"
    // This determines whether we emit .ts or .js output, and whether to emit type stubs
    var is_typescript = false;
    if (module_script) |script| {
        if (script.lang) |lang| {
            if (std.mem.eql(u8, lang, "ts") or std.mem.eql(u8, lang, "typescript")) {
                is_typescript = true;
            }
        }
        // generics attribute implies TypeScript
        if (script.generics != null) {
            is_typescript = true;
        }
    }
    if (instance_script) |script| {
        if (script.lang) |lang| {
            if (std.mem.eql(u8, lang, "ts") or std.mem.eql(u8, lang, "typescript")) {
                is_typescript = true;
            }
        }
        // generics attribute implies TypeScript
        if (script.generics != null) {
            is_typescript = true;
        }
    }

    // Get generics from instance script (if any)
    const instance_generics = if (instance_script) |script| script.generics else null;

    // Extract declared names from scripts FIRST (before emitting any content).
    // This is needed to avoid snippet declaration conflicts and for snippet hoisting.
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

    // Find snippet body ranges for proper scoping.
    // Expressions inside snippet bodies need to be emitted inside the snippet function,
    // not at module scope, so that snippet parameters are in scope.
    var snippet_body_ranges = try findSnippetBodyRanges(allocator, ast.source);
    defer snippet_body_ranges.deinit(allocator);

    // Find if/else branches for type narrowing.
    // Expressions inside {#if condition} blocks should be narrowed by the condition.
    var if_branches = try findIfBranches(allocator, ast.source);
    defer if_branches.deinit(allocator);

    // Find {#each} body ranges for scoping loop variables.
    // Elements inside {#each} blocks need access to the loop variable.
    var each_body_ranges = try findEachBodyRanges(allocator, ast.source);
    defer each_body_ranges.deinit(allocator);

    // Header comment
    try output.appendSlice(allocator, "// Generated from ");
    try output.appendSlice(allocator, ast.file_path);
    try output.appendSlice(allocator, "\n\n");

    // For TypeScript files, emit type imports and rune stubs
    // For JavaScript files, skip these to avoid "X can only be used in TypeScript files" errors
    if (is_typescript) {
        // Svelte type imports
        // Import Component for Svelte 5 type compatibility (makes ComponentProps work)
        // Use a private alias to avoid conflicts with user imports of Component
        // Also import ComponentProps for type-checking component props in templates
        try output.appendSlice(allocator, "import type { Component as __SvelteComponentType__, ComponentProps as __ComponentProps__ } from \"svelte\";\n");
        // Import SvelteHTMLElements for validating HTML element props
        try output.appendSlice(allocator, "import type { SvelteHTMLElements as __SvelteElements__ } from \"svelte/elements\";\n");
        // Note: Snippet is imported later after rune stubs to make it globally available.

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
            \\declare namespace $props {
            \\  function id(): string;
            \\}
            \\declare function $bindable<T>(initial?: T): T;
            \\declare function $inspect<T>(...values: T[]): { with: (fn: (type: 'init' | 'update', ...values: T[]) => void) => void };
            \\declare function $host<T extends HTMLElement>(): T;
            \\
            \\// Snippet type - globally available in Svelte 5 templates
            \\// Re-exported from 'svelte' to avoid "declared but never used" with imports
            \\import type { Snippet } from "svelte";
            \\
            \\// Svelte store auto-subscription stub
            \\// $storeName syntax in Svelte auto-subscribes to the store
            \\// This function extracts the value type from a store's subscribe method
            \\type SvelteStore<T> = { subscribe: (run: (value: T) => any, invalidate?: any) => any };
            \\declare function __svelte_store_get<T>(store: SvelteStore<T>): T;
            \\declare function __svelte_store_get<Store extends SvelteStore<any> | undefined | null>(store: Store): Store extends SvelteStore<infer T> ? T : Store;
            \\
            \\// Array type inference for {#each} blocks
            \\// Extracts element type from arrays, readonly arrays, and ArrayLike types
            \\// Falls back to any[] for unrecognized types (including `any`) to avoid false positives
            \\declare function __svelte_ensureArray<T>(array: T): T extends readonly (infer U)[] ? U[] : T extends ArrayLike<infer U> ? U[] : any[];
            \\
            \\
        );
    } else {
        // For JavaScript files, we still need to mark route_info as used
        _ = route_info;
    }

    // Emit forward declarations for template snippets (hoisting).
    // Svelte hoists snippets so they're visible in both module and instance scripts,
    // even though they're defined in the template. We emit function declarations
    // with proper signatures so TypeScript validates argument counts at call sites.
    // Skip snippets whose names conflict with script declarations.
    // Also skip duplicate snippet names - only hoist the first occurrence.
    var hoisted_snippets: std.StringHashMapUnmanaged(void) = .empty;
    defer hoisted_snippets.deinit(allocator);
    var emitted_snippet_header = false;
    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) {
            if (extractSnippetName(ast.source, node.start, node.end)) |info| {
                // Skip if conflicts with script declarations or already hoisted
                if (!declared_names.contains(info.name) and !hoisted_snippets.contains(info.name)) {
                    try hoisted_snippets.put(allocator, info.name, {});
                    if (!emitted_snippet_header) {
                        try output.appendSlice(allocator, "// Hoisted snippet declarations\n");
                        emitted_snippet_header = true;
                    }
                    // Emit as Snippet-typed variable declaration.
                    // Format: var name: Snippet<[any, ...]> = (..._: any[]) => ({} as any);
                    // This ensures snippets are assignable to Snippet<[T]> props when passed around.
                    // We use `any` parameter types to avoid referencing types not yet imported.
                    if (is_typescript) {
                        try output.appendSlice(allocator, "var ");
                        try output.appendSlice(allocator, info.name);
                        try output.appendSlice(allocator, ": Snippet<[");
                        // Count parameters and emit [any, any, ...] tuple
                        if (info.params) |params| {
                            const param_count = countSnippetParams(params);
                            var param_idx: usize = 0;
                            while (param_idx < param_count) : (param_idx += 1) {
                                if (param_idx > 0) try output.appendSlice(allocator, ", ");
                                try output.appendSlice(allocator, "any");
                            }
                        }
                        try output.appendSlice(allocator, "]> = (..._: any[]) => ({} as any);\n");
                    } else {
                        // JavaScript: emit plain function for arity checking
                        try output.appendSlice(allocator, "function ");
                        try output.appendSlice(allocator, info.name);
                        try output.appendSlice(allocator, "(");
                        if (info.params) |params| {
                            try emitAnyTypedParams(allocator, &output, params, false);
                        }
                        try output.appendSlice(allocator, ") {}\n");
                    }
                }
            }
        }
    }
    if (emitted_snippet_header) {
        try output.appendSlice(allocator, "\n");
    }

    // Emit module script content (if any)
    if (module_script) |script| {
        try output.appendSlice(allocator, "// <script context=\"module\">\n");
        const raw_content = ast.source[script.content_start..script.content_end];
        const filtered = try filterSvelteImports(allocator, raw_content);
        const reactive_transformed = try transformReactiveStatements(allocator, filtered);
        const state_transformed = try transformStateWithTypeAnnotation(allocator, reactive_transformed);
        const content = try transformStoreSubscriptions(allocator, state_transformed);

        try mappings.append(allocator, .{
            .svelte_offset = script.content_start,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(raw_content.len),
        });

        try output.appendSlice(allocator, content);
        try output.appendSlice(allocator, "\n\n");
    }

    // Emit instance script content (if any)
    // For generic components, wrap in a render function to preserve type parameter semantics
    if (instance_script) |script| {
        const raw_content = ast.source[script.content_start..script.content_end];
        const filtered = try filterSvelteImports(allocator, raw_content);
        const reactive_transformed = try transformReactiveStatements(allocator, filtered);
        const state_transformed = try transformStateWithTypeAnnotation(allocator, reactive_transformed);
        const store_transformed = try transformStoreSubscriptions(allocator, state_transformed);

        if (instance_generics) |generics| {
            // For generic components, imports must be at module level (not inside __render).
            // Separate imports from other code, emit imports first, then wrap the rest.
            const separated = try separateImports(allocator, store_transformed);

            // Emit imports at module level
            if (separated.imports.len > 0) {
                try output.appendSlice(allocator, "// <script> imports (hoisted)\n");
                try output.appendSlice(allocator, separated.imports);
                try output.appendSlice(allocator, "\n");
            }

            // Wrap non-import code in async generic render function like svelte2tsx does.
            // async allows await expressions to be valid TypeScript syntax.
            // This ensures type parameters like T in $state<T>(0) trigger proper type checking.
            try output.appendSlice(allocator, "// <script>\nasync function __render<");
            try output.appendSlice(allocator, generics);
            try output.appendSlice(allocator, ">() {\n");

            // Strip 'export' keyword since it's invalid inside a function.
            // The exports are already tracked in $$Exports interface.
            const content = try stripExportKeyword(allocator, separated.other);

            try mappings.append(allocator, .{
                .svelte_offset = script.content_start,
                .ts_offset = @intCast(output.items.len),
                .len = @intCast(raw_content.len),
            });

            try output.appendSlice(allocator, content);
        } else {
            try output.appendSlice(allocator, "// <script>\n");

            try mappings.append(allocator, .{
                .svelte_offset = script.content_start,
                .ts_offset = @intCast(output.items.len),
                .len = @intCast(raw_content.len),
            });

            try output.appendSlice(allocator, store_transformed);
        }
    }

    // Extract props from instance script FIRST (needed for bindable props void statements)
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

    // For generic components, emit template expressions and bindable void statements INSIDE __render
    // For non-generic, emit at module level
    if (instance_generics != null) {
        if (instance_script != null) {
            // For generic components, emit template expressions INSIDE __render
            // so they can access script variables (which are scoped to __render).
            try emitTemplateExpressions(allocator, &ast, &output, &mappings, declared_names, is_typescript, snippet_body_ranges.items, if_branches.items, each_body_ranges.items);

            // Emit void statements for $bindable() props INSIDE __render to suppress "never read" errors.
            // For generic components, props are scoped to __render, so void statements must be inside too.
            try emitBindablePropsVoid(allocator, &output, props.items);

            try output.appendSlice(allocator, "\n}\n\n");
        }
    } else {
        if (instance_script != null) {
            try output.appendSlice(allocator, "\n\n");
        }
        // For non-generic components, emit template expressions at module level
        try emitTemplateExpressions(allocator, &ast, &output, &mappings, declared_names, is_typescript, snippet_body_ranges.items, if_branches.items, each_body_ranges.items);

        // Emit void statements for $bindable() props at module level
        try emitBindablePropsVoid(allocator, &output, props.items);
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

    // Generate component type interfaces
    // For TypeScript files: generate $$Props, $$Slots, $$Exports, $$Bindings interfaces
    // For JavaScript files: skip type interfaces but still emit default export
    if (is_typescript) {
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
            // For generic components, exports are scoped inside __render, so typeof won't work.
            // Use the explicit type if available, otherwise fall back to 'any'.
            if (instance_generics != null) {
                // Generic component: can't use typeof since functions are inside __render
                if (exp.type_repr) |t| {
                    // Skip "typeof X" patterns since X is out of scope
                    if (!std.mem.startsWith(u8, t, "typeof ")) {
                        try output.appendSlice(allocator, t);
                    } else {
                        try output.appendSlice(allocator, "any");
                    }
                } else {
                    try output.appendSlice(allocator, "any");
                }
            } else {
                // Non-generic: typeof references work at module scope
                if (exp.type_repr) |t| {
                    try output.appendSlice(allocator, t);
                } else {
                    try output.appendSlice(allocator, "any");
                }
            }
            try output.appendSlice(allocator, ";\n");
        }
        try output.appendSlice(allocator, "}\n\n");

        // Generate default export using Component interface (Svelte 5 style)
        // This makes ComponentProps<typeof Component> work correctly.
        // The Component interface has the signature: Component<Props, Exports, Bindings>
        // We export both a value (the component function) and a type with the same name
        // (the instance type). When users write `let ref: MyComponent`, TypeScript uses
        // the type alias (ReturnType), which includes the exports, allowing bind:this
        // refs to access exported methods like focusNext().
        try output.appendSlice(allocator,
            \\declare const __SvelteComponent__: __SvelteComponentType__<$$Props, $$Exports, $$Bindings>;
            \\type __SvelteComponent__ = ReturnType<typeof __SvelteComponent__>;
            \\export default __SvelteComponent__;
            \\
        );
    } else {
        // JavaScript mode: emit a default export that's compatible with Svelte's Component type.
        // This ensures JS Svelte files are valid modules that can be imported and used
        // in contexts expecting Component (e.g., Record<string, Component>).
        // We use a function shape since Component<Props, Exports, Bindings> is a function type.
        try output.appendSlice(allocator,
            \\// Component export (JavaScript mode)
            \\// Use a function with bind/call/apply properties to match the Component interface
            \\export default function __SvelteComponent__(internal, props) { return {}; };
            \\
        );
    }

    // Build virtual path - use .js extension for JavaScript, .ts for TypeScript
    // This allows TypeScript to report "Type annotations can only be used in TypeScript files"
    // for scripts that don't have lang="ts"
    const extension = if (is_typescript) ".ts" else ".js";
    const virtual_path = try std.fmt.allocPrint(allocator, "{s}{s}", .{ ast.file_path, extension });

    // Detect TypeScript syntax in JavaScript scripts and emit "Unexpected token" diagnostic.
    // svelte-check reports this at the first `:` or `as` position.
    var ts_syntax_diagnostic: ?Diagnostic = null;
    if (!is_typescript) {
        // Check instance script for TS syntax
        if (instance_script) |script| {
            const content = ast.source[script.content_start..script.content_end];
            if (findFirstTsSyntax(content)) |offset| {
                // Convert offset in script content to position in source file
                const source_offset = script.content_start + offset;
                const pos = offsetToLineCol(ast.source, source_offset);
                ts_syntax_diagnostic = .{
                    .source = .js,
                    .severity = .@"error",
                    .code = null,
                    .message = "Unexpected token",
                    .file_path = ast.file_path,
                    .start_line = pos.line,
                    .start_col = pos.col,
                    .end_line = pos.line,
                    .end_col = pos.col,
                };
            }
        }
        // Check module script if no TS syntax found in instance script
        if (ts_syntax_diagnostic == null) {
            if (module_script) |script| {
                const content = ast.source[script.content_start..script.content_end];
                if (findFirstTsSyntax(content)) |offset| {
                    const source_offset = script.content_start + offset;
                    const pos = offsetToLineCol(ast.source, source_offset);
                    ts_syntax_diagnostic = .{
                        .source = .js,
                        .severity = .@"error",
                        .code = null,
                        .message = "Unexpected token",
                        .file_path = ast.file_path,
                        .start_line = pos.line,
                        .start_col = pos.col,
                        .end_line = pos.line,
                        .end_col = pos.col,
                    };
                }
            }
        }
    }

    return .{
        .original_path = ast.file_path,
        .virtual_path = virtual_path,
        .content = try output.toOwnedSlice(allocator),
        .source_map = .{
            .mappings = try mappings.toOwnedSlice(allocator),
            .svelte_source = ast.source,
        },
        .is_typescript = is_typescript,
        .ts_syntax_diagnostic = ts_syntax_diagnostic,
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
    key_offset: u32, // Svelte source offset for key expression (for source mapping)
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
    generics: ?[]const u8,
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

/// Emit void statements for $bindable() props to suppress "never read" errors.
/// Bindable props are often only written to (reassigned) in script, not read,
/// but the binding is read by the parent component. TypeScript sees writes but
/// no reads, causing false "declared but its value is never read" errors.
fn emitBindablePropsVoid(allocator: std.mem.Allocator, output: *std.ArrayList(u8), props: []const PropInfo) !void {
    var has_bindable_props = false;
    for (props) |prop| {
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
    mappings: *std.ArrayList(SourceMap.Mapping),
    declared_names: std.StringHashMapUnmanaged(void),
    is_typescript: bool,
    snippet_body_ranges: []const SnippetBodyRange,
    if_branches: []const IfBranch,
    each_body_ranges: []const EachBodyRange,
) !void {
    // Track narrowing context to keep if-blocks open across multiple expressions.
    // This enables TypeScript to connect narrowing across declarations and usages.
    var narrowing_ctx = NarrowingContext.init(allocator);
    defer narrowing_ctx.deinit();

    // First pass: collect snippet parameter names.
    // Snippet parameters are only in scope inside the snippet body, not at module scope.
    // We need to exclude them from template_refs to avoid "Cannot find name" errors.
    var snippet_params: std.StringHashMapUnmanaged(void) = .empty;
    defer snippet_params.deinit(allocator);

    // Track template binding names (from {#each} and {@const}).
    // These are declarations, not references, so they shouldn't get void statements.
    // Emitting `void block;` at module scope causes TypeScript to merge types from
    // different if-branches where the same binding name is declared with `var`.
    var template_bindings: std.StringHashMapUnmanaged(void) = .empty;
    defer template_bindings.deinit(allocator);

    for (ast.nodes.items) |node| {
        if (node.kind == .snippet) {
            if (extractSnippetName(ast.source, node.start, node.end)) |info| {
                if (info.params) |params| {
                    // Extract parameter names from the params string (e.g., "val: T, x: number")
                    try extractParamNames(allocator, params, &snippet_params);
                }
            }
        }
    }

    // Collect identifiers referenced in template expressions to emit void statements.
    // This marks variables as "used" so noUnusedLocals works correctly.
    // Value is the Svelte source offset where the identifier was first found.
    var template_refs: std.StringHashMapUnmanaged(u32) = .empty;
    defer template_refs.deinit(allocator);

    // Track current await expression for {:then}/{:catch} type inference.
    // When we see {#await promiseExpr}, we store promiseExpr so that
    // subsequent {:then value} can emit `let value = await (promiseExpr);`
    var current_await_expr: ?[]const u8 = null;

    for (ast.nodes.items) |node| {
        // Skip nodes that are inside snippet bodies - they'll be handled inside the snippet function.
        // This prevents "Cannot find name" errors for snippet parameters used in body expressions.
        // Exception: the snippet node itself, which we need to process to emit the full snippet function.
        if (node.kind != .snippet and isInsideSnippetBody(node.start, snippet_body_ranges)) {
            continue;
        }

        switch (node.kind) {
            .snippet => {
                // Emit the full snippet function with its body expressions.
                // This replaces the empty hoisted declaration with a proper implementation
                // that has snippet parameters in scope for body expressions.
                if (extractSnippetName(ast.source, node.start, node.end)) |info| {
                    // Mark snippet name as "used" to suppress "declared but never read" warnings
                    if (!declared_names.contains(info.name)) {
                        const result = try template_refs.getOrPut(allocator, info.name);
                        if (!result.found_existing) {
                            result.value_ptr.* = node.start;
                        }
                    }

                    // Find the body range for this snippet
                    for (snippet_body_ranges) |range| {
                        if (std.mem.eql(u8, range.name, info.name)) {
                            // Close any open narrowing context before emitting snippet body
                            // Snippets create their own scope, so we can't share the if-block
                            try narrowing_ctx.close(output);
                            // Emit snippet body expressions inside a block.
                            // This creates a scope where snippet parameters are available.
                            try emitSnippetBodyExpressions(
                                allocator,
                                ast,
                                output,
                                mappings,
                                range,
                                is_typescript,
                                if_branches,
                                each_body_ranges,
                                &narrowing_ctx.has_expressions,
                            );
                            break;
                        }
                    }
                }
            },

            .render => {
                // Emit the full render expression to validate snippet call signatures.
                // {@render snippet(args)} → void (snippet(args));
                // This allows TypeScript to check argument count/types against snippet definition.
                const expr = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, expr, node.start, &template_refs);

                if (extractRenderExpression(ast.source, node.start, node.end)) |render_info| {
                    // Use narrowing context to keep if-blocks open across expressions
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        render_info.expr,
                        node.start + render_info.offset,
                        if_branches,
                    );
                }
            },

            .expression => {
                // Extract identifiers AND emit full expression for property access checking.
                // For {searchUsers.pending}, we emit both:
                // - void searchUsers; (for noUnusedLocals)
                // - void (searchUsers.pending); (for property access type-checking)
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                if (extractPlainExpression(ast.source, node.start, node.end)) |expr_info| {
                    // Use narrowing context to keep if-blocks open across expressions
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        expr_info.expr,
                        node.start + expr_info.offset,
                        if_branches,
                    );
                }
            },

            .if_block => {
                // Extract identifiers AND emit full condition for property access checking.
                // Use narrowed expression emission - if this {#if} is nested inside another
                // {#if}/{:else if} block, the enclosing narrowing should apply.
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                if (extractIfExpression(ast.source, node.start, node.end)) |expr_info| {
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        expr_info.expr,
                        node.start + expr_info.offset,
                        if_branches,
                    );
                }
            },

            .key_block => {
                // Extract identifiers AND emit full key expression.
                // Use narrowed expression emission for proper type narrowing.
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                if (extractKeyExpression(ast.source, node.start, node.end)) |expr_info| {
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        expr_info.expr,
                        node.start + expr_info.offset,
                        if_branches,
                    );
                }
            },

            .html => {
                // Extract identifiers AND emit full expression.
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                if (extractHtmlExpression(ast.source, node.start, node.end)) |expr_info| {
                    // Use narrowing context to keep if-blocks open across expressions
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        expr_info.expr,
                        node.start + expr_info.offset,
                        if_branches,
                    );
                }
            },

            .const_tag => {
                // {@const} declares a variable - emit it BEFORE expressions that use it.
                // Extract identifiers from the RHS for noUnusedLocals checking.
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                // Emit the const declaration with narrowing (for {#if} contexts)
                if (extractConstBinding(ast.source, node.start, node.end)) |binding| {
                    // Track binding name to avoid void statement at module scope
                    try template_bindings.put(allocator, binding.name, {});
                    try narrowing_ctx.emitConstBinding(output, binding, node.start, if_branches);
                }
            },

            .debug_tag => {
                // Extract identifiers AND emit debug expressions.
                // Use narrowed expression emission for proper type narrowing.
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);

                if (extractDebugExpression(ast.source, node.start, node.end)) |expr_info| {
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        expr_info.expr,
                        node.start + expr_info.offset,
                        if_branches,
                    );
                }
            },

            .await_block => {
                // {#await promise} or {#await promise then value} - extract identifiers
                // and track the promise expression for subsequent {:then} blocks
                const raw = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, raw, node.start, &template_refs);
                // Store the await expression for {:then}/{:catch} type inference
                if (extractAwaitExpression(ast.source, node.start, node.end)) |await_info| {
                    current_await_expr = await_info.expr;

                    // Emit the await expression for property access checking
                    try narrowing_ctx.emitExpression(
                        output,
                        mappings,
                        await_info.expr,
                        node.start + await_info.offset,
                        if_branches,
                    );

                    // Handle inline {#await promise then value} syntax
                    // This is different from {:then value} which gets its own .then_block node
                    if (extractInlineThenBinding(ast.source, node.start, node.end)) |binding_pattern| {
                        // Close any open narrowing context before emitting standalone declaration
                        try narrowing_ctx.close(output);
                        if (!narrowing_ctx.has_expressions) {
                            try output.appendSlice(allocator, ";// Template expressions\n");
                            narrowing_ctx.has_expressions = true;
                        }
                        const is_destructuring = binding_pattern.len > 0 and (binding_pattern[0] == '{' or binding_pattern[0] == '[');
                        if (is_destructuring) {
                            // Emit: var { x, y } = await (promiseExpr);
                            try output.appendSlice(allocator, "var ");
                            try output.appendSlice(allocator, binding_pattern);
                            try output.appendSlice(allocator, " = await (");
                            try output.appendSlice(allocator, await_info.expr);
                            try output.appendSlice(allocator, ");\n");
                        } else {
                            // Emit: let value = await (promiseExpr);
                            try output.appendSlice(allocator, "let ");
                            try output.appendSlice(allocator, binding_pattern);
                            try output.appendSlice(allocator, " = await (");
                            try output.appendSlice(allocator, await_info.expr);
                            try output.appendSlice(allocator, ");\n");
                        }
                    }
                } else {
                    current_await_expr = null;
                }
            },

            .then_block => {
                // {:then value} - emit binding for the resolved value
                // Don't extract identifiers from the full expression (would extract "then" keyword)
                if (extractThenCatchBinding(ast.source, node.start, node.end, "then")) |binding_pattern| {
                    // Close any open narrowing context before emitting standalone declaration
                    try narrowing_ctx.close(output);
                    if (!narrowing_ctx.has_expressions) {
                        try output.appendSlice(allocator, ";// Template expressions\n");
                        narrowing_ctx.has_expressions = true;
                    }
                    // For destructuring patterns, we need an initializer
                    // For simple identifiers, we can use await to infer the type
                    const is_destructuring = binding_pattern.len > 0 and (binding_pattern[0] == '{' or binding_pattern[0] == '[');
                    if (is_destructuring) {
                        // Emit: var { x, y } = await (promiseExpr);
                        // This infers the type from the promise's resolved value
                        try output.appendSlice(allocator, "var ");
                        try output.appendSlice(allocator, binding_pattern);
                        if (current_await_expr) |await_expr| {
                            try output.appendSlice(allocator, " = await (");
                            try output.appendSlice(allocator, await_expr);
                            try output.appendSlice(allocator, ");\n");
                        } else if (is_typescript) {
                            try output.appendSlice(allocator, " = {} as any;\n");
                        } else {
                            try output.appendSlice(allocator, " = {};\n");
                        }
                    } else {
                        // Emit: let value = await (promiseExpr);
                        // This infers the type from the promise's resolved value
                        try output.appendSlice(allocator, "let ");
                        try output.appendSlice(allocator, binding_pattern);
                        if (current_await_expr) |await_expr| {
                            try output.appendSlice(allocator, " = await (");
                            try output.appendSlice(allocator, await_expr);
                            try output.appendSlice(allocator, ");\n");
                        } else if (is_typescript) {
                            try output.appendSlice(allocator, ": unknown;\n");
                        } else {
                            try output.appendSlice(allocator, ";\n");
                        }
                    }
                }
            },

            .catch_block => {
                // {:catch error} - emit binding for the error
                if (extractThenCatchBinding(ast.source, node.start, node.end, "catch")) |binding_pattern| {
                    // Close any open narrowing context before emitting standalone declaration
                    try narrowing_ctx.close(output);
                    if (!narrowing_ctx.has_expressions) {
                        try output.appendSlice(allocator, ";// Template expressions\n");
                        narrowing_ctx.has_expressions = true;
                    }
                    // For destructuring patterns, we need an initializer
                    // For simple identifiers, type annotation is sufficient
                    const is_destructuring = binding_pattern.len > 0 and (binding_pattern[0] == '{' or binding_pattern[0] == '[');
                    if (is_destructuring) {
                        // Emit: var { x, y } = {} as any;
                        try output.appendSlice(allocator, "var ");
                        try output.appendSlice(allocator, binding_pattern);
                        if (is_typescript) {
                            try output.appendSlice(allocator, " = {} as any;\n");
                        } else {
                            try output.appendSlice(allocator, " = {};\n");
                        }
                    } else {
                        // Emit: let error: unknown;
                        try output.appendSlice(allocator, "let ");
                        try output.appendSlice(allocator, binding_pattern);
                        if (is_typescript) {
                            try output.appendSlice(allocator, ": unknown");
                        }
                        try output.appendSlice(allocator, ";\n");
                    }
                }
            },

            .else_block => {
                // {:else} or {:else if condition} - don't extract "else" keyword
                // If it's {:else if condition}, extract identifiers from the condition
                const expr = ast.source[node.start..node.end];
                // Skip {:else prefix and extract from the rest
                if (std.mem.indexOf(u8, expr, ":else if ")) |idx| {
                    const condition = expr[idx + ":else if ".len ..];
                    try extractIdentifiersFromExpr(allocator, condition, node.start + @as(u32, @intCast(idx + ":else if ".len)), &template_refs);
                }
                // Plain {:else} has no expressions to extract
            },

            .each_block => {
                // Extract identifiers from each expression and emit binding declarations
                const expr = ast.source[node.start..node.end];
                try extractIdentifiersFromExpr(allocator, expr, node.start, &template_refs);

                // Parse and emit each block bindings (item, index variables)
                // Use narrowing context so {#each} inside {#if} shares the same if-block
                if (extractEachBindings(ast.source, node.start, node.end)) |binding| {
                    // Track binding names to avoid emitting void statements for them at module scope.
                    // This prevents TypeScript from merging types across different if-branches.
                    try template_bindings.put(allocator, binding.item_binding, {});
                    if (binding.index_binding) |idx| {
                        try template_bindings.put(allocator, idx, {});
                    }
                    try narrowing_ctx.emitEachBinding(output, mappings, binding, is_typescript, if_branches);
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
                        const result = try template_refs.getOrPut(allocator, tag[0..name_end]);
                        if (!result.found_existing) {
                            // Use node.start as approximate offset (tag name follows '<')
                            result.value_ptr.* = node.start + 1;
                        }
                    }
                }

                for (ast.attributes.items[elem_data.attrs_start..elem_data.attrs_end]) |attr| {
                    // Skip spread attributes
                    if (std.mem.eql(u8, attr.name, "...")) continue;

                    // Handle @attach directive: extract the attached function as "used"
                    // {@attach tooltip(args)} → extract 'tooltip' so import isn't flagged as unused
                    if (std.mem.eql(u8, attr.name, "@attach")) {
                        if (attr.value) |val| {
                            // Value is the full expression like {tooltip(args)}
                            try extractIdentifiersFromExpr(allocator, val, attr.start, &template_refs);
                        }
                        continue;
                    }

                    if (attr.value) |val| {
                        // Expression values are wrapped in {} - may be single or mixed (e.g., "{foo} text")
                        if (val.len > 0 and val[0] == '{') {
                            // Use extractIdentifiersFromAttributeValue to handle mixed values
                            // that contain both expressions and static text
                            try extractIdentifiersFromAttributeValue(allocator, val, attr.start, &template_refs);
                        }
                    }
                    // Shorthand attributes like {foo} use attr name as expression
                    if (attr.value == null and attr.name.len > 0 and !std.mem.startsWith(u8, attr.name, "on:") and !std.mem.startsWith(u8, attr.name, "bind:")) {
                        // This might be a shorthand {foo}
                        const src = ast.source[attr.start..attr.end];
                        if (src.len > 2 and src[0] == '{' and src[src.len - 1] == '}') {
                            try extractIdentifiersFromExpr(allocator, src, attr.start, &template_refs);
                        }
                    }
                    // Shorthand bind:x directive (bind:open means bind:open={open})
                    if (attr.value == null and std.mem.startsWith(u8, attr.name, "bind:")) {
                        const binding_name = attr.name[5..]; // Skip "bind:"
                        if (binding_name.len > 0) {
                            const result = try template_refs.getOrPut(allocator, binding_name);
                            if (!result.found_existing) {
                                result.value_ptr.* = attr.start;
                            }
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
                                    const result = try template_refs.getOrPut(allocator, rest[0..end]);
                                    if (!result.found_existing) {
                                        result.value_ptr.* = attr.start;
                                    }
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
                                try extractIdentifiersFromExpr(allocator, val, attr.start, &template_refs);
                            } else {
                                // Shorthand: class:foo uses variable named foo
                                const result = try template_refs.getOrPut(allocator, class_var);
                                if (!result.found_existing) {
                                    result.value_ptr.* = attr.start;
                                }
                            }
                        }
                    }
                    // style: directive - shorthand style:prop means style:prop={prop}
                    // e.g., style:color, style:--custom-property
                    if (std.mem.startsWith(u8, attr.name, "style:")) {
                        if (attr.value) |val| {
                            // Full syntax: style:prop={expr} or style:prop="css {expr} more"
                            // Use extractIdentifiersFromAttributeValue to handle mixed CSS/expr values
                            if (val.len > 0 and val[0] == '{') {
                                try extractIdentifiersFromAttributeValue(allocator, val, attr.start, &template_refs);
                            }
                        }
                        // Note: shorthand style:foo (without value) is rarely used with variables
                        // because style properties are usually CSS names, not JS identifiers
                    }
                }
            },

            else => {},
        }
    }

    // Emit HTML element props validation using SvelteHTMLElements type assertion
    // This validates that attributes are valid for the element type
    if (is_typescript) {
        var elem_counter: u32 = 0;
        for (ast.nodes.items) |node| {
            if (node.kind != .element) continue;

            // Skip elements inside snippet bodies - they're handled by emitSnippetBodyExpressions
            if (isInsideSnippetBody(node.start, snippet_body_ranges)) continue;

            const elem_data = ast.elements.items[node.data];

            // Skip special Svelte elements that have custom prop handling
            if (std.mem.eql(u8, elem_data.tag_name, "slot")) continue;
            if (std.mem.startsWith(u8, elem_data.tag_name, "svelte:")) continue;
            const attrs = ast.attributes.items[elem_data.attrs_start..elem_data.attrs_end];

            // Collect validatable attributes (exclude Svelte directives and shorthand props)
            var has_validatable_attrs = false;
            for (attrs) |attr| {
                if (isValidatableHtmlAttr(attr.name)) {
                    has_validatable_attrs = true;
                    break;
                }
            }

            if (!has_validatable_attrs) continue;

            // Close any open narrowing context before emitting HTML element validation
            // Element props are validated independently, not within a narrowing block
            try narrowing_ctx.close(output);
            if (!narrowing_ctx.has_expressions) {
                try output.appendSlice(allocator, ";// Template expressions\n");
                narrowing_ctx.has_expressions = true;
            }

            // Find enclosing if branches and {#each} blocks for this element
            // They must be emitted in the correct nested order (by body_start)
            var enclosing = try findAllEnclosingIfBranches(allocator, node.start, if_branches);
            defer enclosing.deinit(allocator);

            var enclosing_each = try findAllEnclosingEachRanges(allocator, node.start, each_body_ranges);
            defer enclosing_each.deinit(allocator);

            // Emit if conditions and each bindings interleaved by their body_start
            const conditions_opened = try emitInterleavedConditionsAndBindings(allocator, output, enclosing.items, enclosing_each.items, is_typescript);

            // Emit: void ({ attr1: value1 } satisfies Partial<__SvelteElements__['tag']>);
            // Use 'satisfies' to check attribute types without declaring variables
            elem_counter += 1;
            try output.appendSlice(allocator, "void ({ ");

            var first_attr = true;
            for (attrs) |attr| {
                if (!isValidatableHtmlAttr(attr.name)) continue;

                if (!first_attr) {
                    try output.appendSlice(allocator, ", ");
                }
                first_attr = false;

                // Check if attribute name needs to be quoted (contains non-identifier chars like -)
                const needs_quote = blk: {
                    for (attr.name) |ch| {
                        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '$') {
                            break :blk true;
                        }
                    }
                    break :blk false;
                };

                // Add source mapping for the attribute name
                // Include the quote in the mapping if present, since TS errors point to the quote
                try mappings.append(allocator, .{
                    .svelte_offset = attr.start,
                    .ts_offset = @intCast(output.items.len),
                    .len = @intCast(attr.name.len + (if (needs_quote) @as(u32, 2) else 0)),
                });
                if (needs_quote) {
                    try output.appendSlice(allocator, "'");
                }
                try output.appendSlice(allocator, attr.name);
                if (needs_quote) {
                    try output.appendSlice(allocator, "'");
                }
                try output.appendSlice(allocator, ": ");

                // Emit value (or undefined for valueless attrs)
                if (attr.value) |val| {
                    if (val.len > 2 and val[0] == '{' and val[val.len - 1] == '}' and isSingleExpression(val)) {
                        // Pure expression value: strip { } and emit the expression
                        const expr = std.mem.trim(u8, val[1 .. val.len - 1], " \t\n\r");
                        try output.appendSlice(allocator, expr);
                    } else if (std.mem.indexOf(u8, val, "{") != null) {
                        // Mixed value with embedded expressions (e.g., "text {expr} more", "{a} {b}")
                        // Emit as template literal to handle multi-line values correctly
                        try emitMixedAttributeValue(allocator, output, mappings, val, attr.start);
                    } else if (std.mem.indexOf(u8, val, "\n") != null) {
                        // Multi-line static value - emit as template literal
                        try output.appendSlice(allocator, "`");
                        for (val) |c| {
                            if (c == '`' or c == '$') {
                                try output.append(allocator, '\\');
                            }
                            try output.append(allocator, c);
                        }
                        try output.appendSlice(allocator, "`");
                    } else {
                        // Static string value - check if it should be emitted as a number
                        // for numeric HTML attributes (tabindex, aria-level, etc.)
                        if (isNumericHtmlAttr(attr.name) and isIntegerString(val)) {
                            // Emit as numeric literal to match type definitions
                            try output.appendSlice(allocator, val);
                        } else {
                            // Emit as string literal
                            try output.appendSlice(allocator, "\"");
                            try output.appendSlice(allocator, val);
                            try output.appendSlice(allocator, "\"");
                        }
                    }
                } else {
                    // Check if this is a shorthand prop like {foo}
                    const src = ast.source[attr.start..attr.end];
                    if (src.len > 2 and src[0] == '{' and src[src.len - 1] == '}') {
                        // Shorthand: {foo} → foo: foo
                        const expr = std.mem.trim(u8, src[1 .. src.len - 1], " \t\n\r");
                        try output.appendSlice(allocator, expr);
                    } else {
                        // Valueless attribute (e.g., disabled, hidden)
                        try output.appendSlice(allocator, "true");
                    }
                }
            }

            try output.appendSlice(allocator, " } satisfies Partial<__SvelteElements__['");
            try output.appendSlice(allocator, elem_data.tag_name);
            try output.appendSlice(allocator, "']>);");

            // Close all opened if blocks
            var k: u32 = 0;
            while (k < conditions_opened) : (k += 1) {
                try output.appendSlice(allocator, " }");
            }
            try output.appendSlice(allocator, "\n");
        }
    }

    // Component props validation is temporarily disabled.
    // The approach of using __ComponentProps__<typeof Component> produces false positives
    // because it requires ALL props, including those with defaults. Svelte's actual
    // component prop checking is more nuanced and handles optional props correctly.
    // TODO: Implement proper component props validation that:
    // - Distinguishes required vs optional props
    // - Reports "unknown property" errors like svelte-check does
    // - Handles generic components correctly
    const enable_component_props_validation = false;
    if (enable_component_props_validation and is_typescript) {
        for (ast.nodes.items) |node| {
            if (node.kind != .component) continue;

            // Skip components inside snippet bodies - they're handled by emitSnippetBodyExpressions
            if (isInsideSnippetBody(node.start, snippet_body_ranges)) continue;

            const elem_data = ast.elements.items[node.data];
            const tag_name = elem_data.tag_name;

            // Skip svelte: special elements
            if (std.mem.startsWith(u8, tag_name, "svelte:")) continue;

            const attrs = ast.attributes.items[elem_data.attrs_start..elem_data.attrs_end];

            // Collect validatable component props (exclude Svelte directives)
            var has_validatable_props = false;
            for (attrs) |attr| {
                if (isValidatableComponentProp(attr.name)) {
                    has_validatable_props = true;
                    break;
                }
            }

            if (!has_validatable_props) continue;

            // Close any open narrowing context before emitting component props validation
            try narrowing_ctx.close(output);
            if (!narrowing_ctx.has_expressions) {
                try output.appendSlice(allocator, ";// Template expressions\n");
                narrowing_ctx.has_expressions = true;
            }

            // Find enclosing if branches for this component and emit if wrappers
            // Skip if-branches that are inside snippet bodies since their conditions
            // reference variables that are only in scope within the snippet
            var enclosing = try findAllEnclosingIfBranches(allocator, node.start, if_branches);
            defer enclosing.deinit(allocator);

            // Filter out branches inside snippet bodies
            var filtered: std.ArrayList(IfBranch) = .empty;
            defer filtered.deinit(allocator);
            for (enclosing.items) |branch| {
                const branch_inside_snippet = isInsideSnippetBody(branch.body_start, snippet_body_ranges);
                if (!branch_inside_snippet) {
                    try filtered.append(allocator, branch);
                }
            }

            const conditions_opened = try emitIfConditionOpeners(allocator, output, filtered.items);

            // Emit: ((_: __ComponentProps__<typeof ComponentName>) => {})({ prop1: value1, ... });
            // This validates props without actually calling the component
            try output.appendSlice(allocator, "((_: __ComponentProps__<typeof ");
            try output.appendSlice(allocator, tag_name);
            try output.appendSlice(allocator, ">) => {})({ ");

            var first_prop = true;
            for (attrs) |attr| {
                if (!isValidatableComponentProp(attr.name)) continue;

                if (!first_prop) {
                    try output.appendSlice(allocator, ", ");
                }
                first_prop = false;

                // Check if prop name needs to be quoted
                const needs_quote = blk: {
                    for (attr.name) |ch| {
                        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '$') {
                            break :blk true;
                        }
                    }
                    break :blk false;
                };

                // Add source mapping for the prop name
                try mappings.append(allocator, .{
                    .svelte_offset = attr.start,
                    .ts_offset = @intCast(output.items.len),
                    .len = @intCast(attr.name.len + (if (needs_quote) @as(u32, 2) else 0)),
                });
                if (needs_quote) {
                    try output.appendSlice(allocator, "'");
                }
                try output.appendSlice(allocator, attr.name);
                if (needs_quote) {
                    try output.appendSlice(allocator, "'");
                }
                try output.appendSlice(allocator, ": ");

                // Emit value
                if (attr.value) |val| {
                    if (val.len > 2 and val[0] == '{' and val[val.len - 1] == '}' and isSingleExpression(val)) {
                        // Pure expression value: strip { } and emit the expression
                        const expr = std.mem.trim(u8, val[1 .. val.len - 1], " \t\n\r");
                        try output.appendSlice(allocator, expr);
                    } else if (std.mem.indexOf(u8, val, "{") != null) {
                        // Mixed value with embedded expressions
                        try emitMixedAttributeValue(allocator, output, mappings, val, attr.start);
                    } else if (std.mem.indexOf(u8, val, "\n") != null) {
                        // Multi-line static value - emit as template literal
                        try output.appendSlice(allocator, "`");
                        for (val) |c| {
                            if (c == '`' or c == '$') {
                                try output.append(allocator, '\\');
                            }
                            try output.append(allocator, c);
                        }
                        try output.appendSlice(allocator, "`");
                    } else {
                        // Static string value
                        try output.appendSlice(allocator, "\"");
                        try output.appendSlice(allocator, val);
                        try output.appendSlice(allocator, "\"");
                    }
                } else {
                    // Check if this is a shorthand prop like {foo}
                    const src = ast.source[attr.start..attr.end];
                    if (src.len > 2 and src[0] == '{' and src[src.len - 1] == '}') {
                        // Shorthand: {foo} → foo: foo
                        const expr = std.mem.trim(u8, src[1 .. src.len - 1], " \t\n\r");
                        try output.appendSlice(allocator, expr);
                    } else {
                        // Valueless prop (e.g., <Component disabled />)
                        try output.appendSlice(allocator, "true");
                    }
                }
            }

            try output.appendSlice(allocator, " });");

            // Close all opened if blocks
            var k: u32 = 0;
            while (k < conditions_opened) : (k += 1) {
                try output.appendSlice(allocator, " }");
            }
            try output.appendSlice(allocator, "\n");
        }
    }

    // Close any open narrowing context before the final sections
    try narrowing_ctx.close(output);

    // Fallback: scan template source directly for {#each} patterns not captured by AST
    // The parser sometimes misses {#each} blocks inside component elements
    try scanTemplateForEachBlocks(allocator, ast, output, mappings, &template_refs, &narrowing_ctx.has_expressions, is_typescript, snippet_body_ranges, if_branches);

    // Emit void statements for template-referenced identifiers
    // This marks them as "used" for noUnusedLocals checking
    var iter = template_refs.iterator();
    while (iter.next()) |entry| {
        const name = entry.key_ptr.*;
        const svelte_offset = entry.value_ptr.*;
        // Skip snippet parameters - they're only in scope inside the snippet function
        if (snippet_params.contains(name)) continue;
        // Skip keywords and built-ins, UNLESS the name is declared in script.
        // User imports can shadow built-in names (e.g., `import Map from './icons/map'`),
        // so if a name is declared in script, we should emit void for it.
        if (isJsKeywordOrBuiltin(name) and !declared_names.contains(name)) continue;
        // Skip template bindings ({#each}, {@const}) - they're declarations, not references.
        // Emitting void at module scope causes TypeScript to merge types across branches.
        if (template_bindings.contains(name)) continue;

        if (!narrowing_ctx.has_expressions) {
            try output.appendSlice(allocator, ";// Template expressions\n");
            narrowing_ctx.has_expressions = true;
        }
        try output.appendSlice(allocator, "void ");
        // Add source mapping so "Cannot find name" errors point to the template location
        try mappings.append(allocator, .{
            .svelte_offset = svelte_offset,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(name.len),
        });
        try output.appendSlice(allocator, name);
        try output.appendSlice(allocator, ";\n");
    }

    if (narrowing_ctx.has_expressions) {
        try output.appendSlice(allocator, "\n");
    }
}

/// Emits a mixed attribute value (e.g., "text {expr} more") as a template literal.
/// Converts Svelte's {expr} syntax to JavaScript's ${expr} template literal syntax.
/// Handles multi-line values correctly since template literals allow newlines.
fn emitMixedAttributeValue(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
    val: []const u8,
    attr_start: u32,
) !void {
    _ = mappings;
    _ = attr_start;
    try output.appendSlice(allocator, "`");

    var i: usize = 0;
    while (i < val.len) {
        const c = val[i];

        if (c == '{') {
            // Find matching closing brace
            const expr_start = i + 1;
            var depth: usize = 1;
            var j = expr_start;
            while (j < val.len and depth > 0) : (j += 1) {
                switch (val[j]) {
                    '{' => depth += 1,
                    '}' => depth -= 1,
                    '"', '\'' => {
                        // Skip string literal
                        const quote = val[j];
                        j += 1;
                        while (j < val.len and val[j] != quote) : (j += 1) {
                            if (val[j] == '\\' and j + 1 < val.len) j += 1;
                        }
                    },
                    '`' => {
                        // Skip template literal (simplified - doesn't handle nested ${})
                        j += 1;
                        while (j < val.len and val[j] != '`') : (j += 1) {
                            if (val[j] == '\\' and j + 1 < val.len) j += 1;
                        }
                    },
                    else => {},
                }
            }

            if (depth == 0) {
                const expr_end = j - 1;
                try output.appendSlice(allocator, "${");
                try output.appendSlice(allocator, val[expr_start..expr_end]);
                try output.appendSlice(allocator, "}");
                i = j;
            } else {
                // Malformed - just emit the brace
                try output.append(allocator, c);
                i += 1;
            }
        } else if (c == '`' or c == '$') {
            // Escape backticks and $ in template literals
            try output.append(allocator, '\\');
            try output.append(allocator, c);
            i += 1;
        } else {
            try output.append(allocator, c);
            i += 1;
        }
    }

    try output.appendSlice(allocator, "`");
}

/// Checks if an attribute value is a single expression (just "{expr}").
/// Returns false for mixed values like "{a} {b}" or "text {expr}".
fn isSingleExpression(val: []const u8) bool {
    if (val.len < 2 or val[0] != '{') return false;

    var depth: usize = 1;
    var i: usize = 1;
    while (i < val.len and depth > 0) {
        const c = val[i];

        // Skip line comments (// ... until newline)
        // This prevents apostrophes in comments like "Don't" from being parsed as strings
        if (c == '/' and i + 1 < val.len and val[i + 1] == '/') {
            i += 2;
            while (i < val.len and val[i] != '\n') : (i += 1) {}
            if (i < val.len) i += 1; // skip the newline
            continue;
        }

        // Skip block comments (/* ... */)
        if (c == '/' and i + 1 < val.len and val[i + 1] == '*') {
            i += 2;
            while (i + 1 < val.len) {
                if (val[i] == '*' and val[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        switch (c) {
            '{' => {
                depth += 1;
                i += 1;
            },
            '}' => {
                depth -= 1;
                i += 1;
            },
            '"', '\'' => {
                // Skip string literal
                const quote = c;
                i += 1;
                while (i < val.len and val[i] != quote) : (i += 1) {
                    if (val[i] == '\\' and i + 1 < val.len) i += 1;
                }
                if (i < val.len) i += 1; // skip closing quote
            },
            '`' => {
                // Skip template literal (simplified - doesn't handle ${} expressions)
                i += 1;
                while (i < val.len and val[i] != '`') : (i += 1) {
                    if (val[i] == '\\' and i + 1 < val.len) i += 1;
                }
                if (i < val.len) i += 1; // skip closing backtick
            },
            else => i += 1,
        }
    }

    // After the first expression closes, there should be nothing left (trimmed)
    if (depth != 0) return false;
    const remaining = std.mem.trim(u8, val[i..], " \t\n\r");
    return remaining.len == 0;
}

/// Emits component props validation code for a single component.
/// Generates: ((_: __ComponentProps__<typeof TagName>) => {})({ prop1: val1, prop2: val2 });
/// This is used both for top-level components and components inside snippets/each blocks.
fn emitComponentPropsValidation(
    allocator: std.mem.Allocator,
    source: []const u8,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
    tag_name: []const u8,
    attrs_start: u32,
    attrs_end: u32,
    attributes: []const AttributeData,
) !void {
    const attrs = attributes[attrs_start..attrs_end];

    // Collect validatable component props (exclude Svelte directives)
    var has_validatable_props = false;
    for (attrs) |attr| {
        if (isValidatableComponentProp(attr.name)) {
            has_validatable_props = true;
            break;
        }
    }

    if (!has_validatable_props) return;

    // Emit: ((_: __ComponentProps__<typeof ComponentName>) => {})({ prop1: value1, ... });
    try output.appendSlice(allocator, "((_: __ComponentProps__<typeof ");
    try output.appendSlice(allocator, tag_name);
    try output.appendSlice(allocator, ">) => {})({ ");

    var first_prop = true;
    for (attrs) |attr| {
        if (!isValidatableComponentProp(attr.name)) continue;

        if (!first_prop) {
            try output.appendSlice(allocator, ", ");
        }
        first_prop = false;

        // Check if prop name needs to be quoted
        const needs_quote = blk: {
            for (attr.name) |ch| {
                if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '$') {
                    break :blk true;
                }
            }
            break :blk false;
        };

        // Add source mapping for the prop name
        try mappings.append(allocator, .{
            .svelte_offset = attr.start,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(attr.name.len + (if (needs_quote) @as(u32, 2) else 0)),
        });
        if (needs_quote) {
            try output.appendSlice(allocator, "'");
        }
        try output.appendSlice(allocator, attr.name);
        if (needs_quote) {
            try output.appendSlice(allocator, "'");
        }
        try output.appendSlice(allocator, ": ");

        // Emit value
        if (attr.value) |val| {
            if (val.len > 2 and val[0] == '{' and val[val.len - 1] == '}' and isSingleExpression(val)) {
                // Pure expression value: strip { } and emit the expression
                const expr = std.mem.trim(u8, val[1 .. val.len - 1], " \t\n\r");
                try output.appendSlice(allocator, expr);
            } else if (std.mem.indexOf(u8, val, "{") != null) {
                // Mixed value with embedded expressions
                try emitMixedAttributeValue(allocator, output, mappings, val, attr.start);
            } else if (std.mem.indexOf(u8, val, "\n") != null) {
                // Multi-line static value - emit as template literal
                try output.appendSlice(allocator, "`");
                for (val) |c| {
                    if (c == '`' or c == '$') {
                        try output.append(allocator, '\\');
                    }
                    try output.append(allocator, c);
                }
                try output.appendSlice(allocator, "`");
            } else {
                // Static string value
                try output.appendSlice(allocator, "\"");
                try output.appendSlice(allocator, val);
                try output.appendSlice(allocator, "\"");
            }
        } else {
            // Check if this is a shorthand prop like {foo}
            const src = source[attr.start..attr.end];
            if (src.len > 2 and src[0] == '{' and src[src.len - 1] == '}') {
                // Shorthand: {foo} → foo: foo
                const expr = std.mem.trim(u8, src[1 .. src.len - 1], " \t\n\r");
                try output.appendSlice(allocator, expr);
            } else {
                // Valueless prop (e.g., <Component disabled />)
                try output.appendSlice(allocator, "true");
            }
        }
    }

    try output.appendSlice(allocator, " });\n");
}

/// Extracts identifier references from an attribute value that may contain mixed text and expressions.
/// For example, in `class="{foo} static-class"`, this extracts identifiers only from `{foo}`,
/// not from the static text "static-class". Handles multiple expressions like `{a} text {b}`.
fn extractIdentifiersFromAttributeValue(
    allocator: std.mem.Allocator,
    val: []const u8,
    base_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < val.len) {
        if (val[i] == '{') {
            // Find matching closing brace
            const expr_start = i;
            var depth: usize = 1;
            var j = i + 1;
            while (j < val.len and depth > 0) {
                const c = val[j];
                switch (c) {
                    '{' => {
                        depth += 1;
                        j += 1;
                    },
                    '}' => {
                        depth -= 1;
                        j += 1;
                    },
                    '/' => {
                        // Handle line comments - apostrophes inside shouldn't be treated as strings
                        if (j + 1 < val.len and val[j + 1] == '/') {
                            j += 2;
                            while (j < val.len and val[j] != '\n') : (j += 1) {}
                        } else if (j + 1 < val.len and val[j + 1] == '*') {
                            // Handle block comments
                            j += 2;
                            while (j + 1 < val.len) {
                                if (val[j] == '*' and val[j + 1] == '/') {
                                    j += 2;
                                    break;
                                }
                                j += 1;
                            }
                        } else {
                            j += 1;
                        }
                    },
                    '"', '\'' => {
                        // Skip string literal
                        const quote = c;
                        j += 1;
                        while (j < val.len and val[j] != quote) : (j += 1) {
                            if (val[j] == '\\' and j + 1 < val.len) j += 1;
                        }
                        if (j < val.len) j += 1; // Skip closing quote
                    },
                    '`' => {
                        // Skip template literal
                        j += 1;
                        while (j < val.len and val[j] != '`') : (j += 1) {
                            if (val[j] == '\\' and j + 1 < val.len) j += 1;
                        }
                        if (j < val.len) j += 1; // Skip closing backtick
                    },
                    else => j += 1,
                }
            }

            if (depth == 0) {
                // Extract identifiers only from the expression content (between { and })
                const expr = val[expr_start..j];
                try extractIdentifiersFromExpr(allocator, expr, base_offset + @as(u32, @intCast(expr_start)), refs);
                i = j;
            } else {
                i += 1;
            }
        } else {
            // Skip static text (not inside {})
            i += 1;
        }
    }
}

/// Extracts identifier references from a template expression string.
/// Handles expressions like {foo}, {foo.bar}, {foo + bar}, onclick={handler}, etc.
/// Skips identifiers that are arrow function parameters (e.g., `e` in `e => e`).
/// `base_offset` is the Svelte source offset of the start of `expr`.
const ParamRegion = struct {
    start: usize,
    end: usize,
};

fn extractIdentifiersFromExpr(
    allocator: std.mem.Allocator,
    expr: []const u8,
    base_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
) std.mem.Allocator.Error!void {
    // First pass: collect arrow function parameter names AND parameter regions.
    // Names are used to exclude references in the function body.
    // Regions are used to skip extracting identifiers from the parameter list itself.
    var arrow_params: std.StringHashMapUnmanaged(void) = .empty;
    defer arrow_params.deinit(allocator);
    var param_regions: std.ArrayList(ParamRegion) = .empty;
    defer param_regions.deinit(allocator);
    try collectArrowFunctionParamsAndRegions(allocator, expr, &arrow_params, &param_regions);

    // First pass: also collect locally declared variables (const/let/var declarations)
    // These are scoped to the expression and shouldn't be extracted as template refs.
    var local_decls: std.StringHashMapUnmanaged(void) = .empty;
    defer local_decls.deinit(allocator);
    try collectLocalDeclarations(allocator, expr, &local_decls);

    // Use internal function with exclusion context for recursive calls
    try extractIdentifiersFromExprWithContext(allocator, expr, base_offset, refs, &arrow_params, &local_decls, param_regions.items);
}

/// Internal version that accepts pre-collected exclusion sets for recursive calls.
/// This ensures local declarations from outer scopes are respected in template literal interpolations.
fn extractIdentifiersFromExprWithContext(
    allocator: std.mem.Allocator,
    expr: []const u8,
    base_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
    arrow_params: *const std.StringHashMapUnmanaged(void),
    local_decls: *const std.StringHashMapUnmanaged(void),
    param_regions: []const ParamRegion,
) std.mem.Allocator.Error!void {
    // Track function scope depth to skip identifiers inside function bodies.
    // Variables inside IIFEs or callbacks should not be extracted to module scope.
    var func_depth: u32 = 0;
    var saw_arrow = false;

    var i: usize = 0;
    while (i < expr.len) {
        const c = expr[i];

        // Check if we're inside a parameter region - skip entirely
        var in_param_region = false;
        for (param_regions) |region| {
            if (i >= region.start and i < region.end) {
                in_param_region = true;
                i = region.end;
                break;
            }
        }
        if (in_param_region) continue;

        // Handle strings - for template literals, extract from interpolations
        if (c == '"' or c == '\'') {
            i = skipStringLiteral(expr, i);
            saw_arrow = false;
            continue;
        }
        if (c == '`') {
            // Only extract from template literals if we're at top level
            if (func_depth == 0) {
                i = try skipStringLiteralAndExtractWithContext(allocator, expr, i, base_offset, refs, arrow_params, local_decls);
            } else {
                i = skipStringLiteral(expr, i);
            }
            saw_arrow = false;
            continue;
        }

        // Skip comments and regex literals
        if (i + 1 < expr.len and c == '/') {
            if (expr[i + 1] == '/') {
                while (i < expr.len and expr[i] != '\n') : (i += 1) {}
                saw_arrow = false;
                continue;
            }
            if (expr[i + 1] == '*') {
                i += 2;
                while (i + 1 < expr.len and !(expr[i] == '*' and expr[i + 1] == '/')) : (i += 1) {}
                if (i + 1 < expr.len) i += 2;
                saw_arrow = false;
                continue;
            }
            // Check for regex literal: /pattern/flags
            // Regex follows operators, punctuation, or start - not expressions
            if (couldBeRegex(expr, i)) {
                i = skipRegexLiteral(expr, i);
                saw_arrow = false;
                continue;
            }
        }

        // Track arrow => which starts a function scope
        if (c == '=' and i + 1 < expr.len and expr[i + 1] == '>') {
            saw_arrow = true;
            i += 2;
            continue;
        }

        // Track function body entry
        if (c == '{') {
            if (saw_arrow) {
                func_depth += 1;
                saw_arrow = false;
            } else if (func_depth > 0) {
                func_depth += 1;
            }
            i += 1;
            continue;
        }

        if (c == '}') {
            if (func_depth > 0) {
                func_depth -= 1;
            }
            i += 1;
            saw_arrow = false;
            continue;
        }

        // Non-brace token after arrow: single-expression body, no scope change needed
        if (saw_arrow and !std.ascii.isWhitespace(c)) {
            saw_arrow = false;
        }

        // Check for identifier start
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            const start = i;
            while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '$')) : (i += 1) {}
            const ident = expr[start..i];

            // Track 'function' keyword - skip to its body and increase scope depth
            if (std.mem.eql(u8, ident, "function")) {
                // Skip to opening paren of parameters
                while (i < expr.len and expr[i] != '(') : (i += 1) {}
                if (i < expr.len) {
                    var paren_depth: u32 = 1;
                    i += 1;
                    while (i < expr.len and paren_depth > 0) {
                        if (expr[i] == '(') paren_depth += 1;
                        if (expr[i] == ')') paren_depth -= 1;
                        i += 1;
                    }
                    while (i < expr.len and std.ascii.isWhitespace(expr[i])) : (i += 1) {}
                    if (i < expr.len and expr[i] == '{') {
                        func_depth += 1;
                        i += 1;
                    }
                }
                continue;
            }

            // Skip identifiers inside function bodies - they're scoped to the function
            if (func_depth > 0) continue;

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

            // Skip arrow function parameters - they're scoped to the function, not module scope
            if (arrow_params.contains(ident)) continue;

            // Skip locally declared variables (const/let/var inside callbacks)
            if (local_decls.contains(ident)) continue;

            // Skip JavaScript keywords that may appear in expressions
            if (isJavaScriptKeyword(ident)) continue;

            // Skip object literal property keys (identifier followed by : in object context)
            // Pattern: { key: value } - 'key' is not a variable reference
            // But NOT: ternary expressions (a ? b : c) or type annotations
            // Check if followed by : (with optional whitespace)
            var peek_idx = i;
            while (peek_idx < expr.len and std.ascii.isWhitespace(expr[peek_idx])) : (peek_idx += 1) {}
            if (peek_idx < expr.len and expr[peek_idx] == ':') {
                // Check if this looks like an object literal key context
                // Look backward for { or , that would indicate object literal
                if (isObjectLiteralKeyContext(expr, start)) {
                    continue; // Skip this identifier - it's an object key, not a reference
                }
            }

            // Store identifier reference with its Svelte source offset
            // Only store the first occurrence (don't overwrite with later occurrences)
            if (ident.len > 0) {
                const offset = base_offset + @as(u32, @intCast(start));
                const result = try refs.getOrPut(allocator, ident);
                if (!result.found_existing) {
                    result.value_ptr.* = offset;
                }

                // For $prefixed identifiers (Svelte store subscriptions like $showNudge),
                // also add the unprefixed name (showNudge). The store variable is declared
                // in script, but only the $prefixed form is used in templates. Adding both
                // ensures the original variable is marked as "used".
                if (ident[0] == '$' and ident.len > 1) {
                    const unprefixed_result = try refs.getOrPut(allocator, ident[1..]);
                    if (!unprefixed_result.found_existing) {
                        unprefixed_result.value_ptr.* = offset + 1; // +1 to skip the $
                    }
                }
            }
            continue;
        }

        i += 1;
    }
}

/// Collects arrow function parameter names AND parameter regions from an expression.
/// Names are used to exclude references in function bodies.
/// Regions mark the parameter list positions to skip during identifier extraction.
/// Handles patterns: e => ..., (e) => ..., (a, b) => ..., (e: Type) => ..., async (...) => ...
fn collectArrowFunctionParamsAndRegions(
    allocator: std.mem.Allocator,
    expr: []const u8,
    params: *std.StringHashMapUnmanaged(void),
    regions: *std.ArrayList(ParamRegion),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < expr.len) {
        const c = expr[i];

        // Skip string literals
        if (c == '"' or c == '\'' or c == '`') {
            i = skipStringLiteral(expr, i);
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

        // Look for arrow function patterns
        // Pattern 1: identifier followed by => (e.g., e => e) or async (...) => ...
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            const ident_start = i;
            while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '$')) : (i += 1) {}
            const ident = expr[ident_start..i];

            // Skip whitespace
            var j = i;
            while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}

            // Check for =>
            if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                // This identifier is an arrow function parameter
                try params.put(allocator, ident, {});
                // Record the region covering just the identifier
                try regions.append(allocator, .{ .start = ident_start, .end = i });
            }
            // Check for async (...) => pattern
            else if (std.mem.eql(u8, ident, "async") and j < expr.len and expr[j] == '(') {
                // This is async arrow function - process the parenthesized params
                const paren_start = j;
                j += 1;
                var depth: u32 = 1;
                while (j < expr.len and depth > 0) {
                    if (expr[j] == '(') depth += 1;
                    if (expr[j] == ')') depth -= 1;
                    if (depth > 0) j += 1;
                }
                const paren_end = j;
                if (j < expr.len) j += 1; // Skip closing )

                // Skip whitespace
                while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}

                // Check for => after the parentheses
                if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                    try extractArrowParamsFromParens(allocator, expr[paren_start + 1 .. paren_end], params);
                    // Record region covering the parenthesized params (including parens)
                    try regions.append(allocator, .{ .start = paren_start, .end = paren_end + 1 });
                }
                i = j;
            }
            continue;
        }

        // Pattern 2: parenthesized parameters followed by => (e.g., (e) => ..., (a, b) => ...)
        // Also handles function call arguments that contain arrow functions
        if (c == '(') {
            const paren_start = i;
            i += 1;
            var depth: u32 = 1;
            while (i < expr.len and depth > 0) {
                if (expr[i] == '(') depth += 1;
                if (expr[i] == ')') depth -= 1;
                if (depth > 0) i += 1;
            }
            const paren_end = i;
            if (i < expr.len) i += 1; // Skip closing )

            // Skip whitespace
            var j = i;
            while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}

            // Handle TypeScript return type annotation: (params): ReturnType => body
            var return_type_end: usize = paren_end;
            if (j < expr.len and expr[j] == ':') {
                j = skipReturnTypeAnnotation(expr, j);
                return_type_end = j;
            }

            // Check for => after the parentheses (or after return type)
            if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                // Extract parameter names from inside parentheses
                try extractArrowParamsFromParens(allocator, expr[paren_start + 1 .. paren_end], params);
                // Record region covering the params AND return type annotation (to skip `c` in `c is Type`)
                try regions.append(allocator, .{ .start = paren_start, .end = return_type_end + 1 });
            } else {
                // Not an arrow function - but the content might contain arrow functions
                // Recursively scan the parenthesized content for nested arrow functions
                // Adjust the region offsets to be relative to the outer expression
                const inner_start = paren_start + 1;
                try collectArrowFunctionParamsAndRegionsWithOffset(allocator, expr[inner_start..paren_end], inner_start, params, regions);
            }
            continue;
        }

        i += 1;
    }
}

/// Same as collectArrowFunctionParamsAndRegions but with an offset for nested calls
fn collectArrowFunctionParamsAndRegionsWithOffset(
    allocator: std.mem.Allocator,
    expr: []const u8,
    offset: usize,
    params: *std.StringHashMapUnmanaged(void),
    regions: *std.ArrayList(ParamRegion),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < expr.len) {
        const c = expr[i];

        // Skip string literals
        if (c == '"' or c == '\'' or c == '`') {
            i = skipStringLiteral(expr, i);
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

        // Look for arrow function patterns
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            const ident_start = i;
            while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '$')) : (i += 1) {}
            const ident = expr[ident_start..i];

            var j = i;
            while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}

            if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                try params.put(allocator, ident, {});
                try regions.append(allocator, .{ .start = offset + ident_start, .end = offset + i });
            } else if (std.mem.eql(u8, ident, "async") and j < expr.len and expr[j] == '(') {
                const paren_start = j;
                j += 1;
                var depth: u32 = 1;
                while (j < expr.len and depth > 0) {
                    if (expr[j] == '(') depth += 1;
                    if (expr[j] == ')') depth -= 1;
                    if (depth > 0) j += 1;
                }
                const paren_end = j;
                if (j < expr.len) j += 1;
                while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}
                // Handle TypeScript return type annotation
                var async_return_type_end: usize = paren_end;
                if (j < expr.len and expr[j] == ':') {
                    j = skipReturnTypeAnnotation(expr, j);
                    async_return_type_end = j;
                }
                if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                    try extractArrowParamsFromParens(allocator, expr[paren_start + 1 .. paren_end], params);
                    try regions.append(allocator, .{ .start = offset + paren_start, .end = offset + async_return_type_end + 1 });
                }
                i = j;
            }
            continue;
        }

        if (c == '(') {
            const paren_start = i;
            i += 1;
            var depth: u32 = 1;
            while (i < expr.len and depth > 0) {
                if (expr[i] == '(') depth += 1;
                if (expr[i] == ')') depth -= 1;
                if (depth > 0) i += 1;
            }
            const paren_end = i;
            if (i < expr.len) i += 1;

            var j = i;
            while (j < expr.len and std.ascii.isWhitespace(expr[j])) : (j += 1) {}

            // Handle TypeScript return type annotation
            var return_type_end2: usize = paren_end;
            if (j < expr.len and expr[j] == ':') {
                j = skipReturnTypeAnnotation(expr, j);
                return_type_end2 = j;
            }

            if (j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
                try extractArrowParamsFromParens(allocator, expr[paren_start + 1 .. paren_end], params);
                try regions.append(allocator, .{ .start = offset + paren_start, .end = offset + return_type_end2 + 1 });
            } else {
                const inner_start = paren_start + 1;
                try collectArrowFunctionParamsAndRegionsWithOffset(allocator, expr[inner_start..paren_end], offset + inner_start, params, regions);
            }
            continue;
        }

        i += 1;
    }
}

/// Collects locally declared variables (const/let/var) from an expression.
/// These are scoped to the expression and shouldn't be extracted as template refs.
fn collectLocalDeclarations(
    allocator: std.mem.Allocator,
    expr: []const u8,
    decls: *std.StringHashMapUnmanaged(void),
) std.mem.Allocator.Error!void {
    // Only collect declarations at the top-level scope of the expression.
    // Declarations inside function bodies (arrow functions, IIFEs, etc.) are scoped
    // to that function and should not be added to module-level local_decls.
    var i: usize = 0;
    var func_depth: u32 = 0; // Track nested function scopes
    var saw_arrow = false; // Track if we just saw => and should enter function scope on next {

    while (i < expr.len) {
        const c = expr[i];

        // Skip string literals
        if (c == '"' or c == '\'' or c == '`') {
            i = skipStringLiteral(expr, i);
            saw_arrow = false;
            continue;
        }

        // Skip comments
        if (i + 1 < expr.len and c == '/') {
            if (expr[i + 1] == '/') {
                while (i < expr.len and expr[i] != '\n') : (i += 1) {}
                saw_arrow = false;
                continue;
            }
            if (expr[i + 1] == '*') {
                i += 2;
                while (i + 1 < expr.len and !(expr[i] == '*' and expr[i + 1] == '/')) : (i += 1) {}
                if (i + 1 < expr.len) i += 2;
                saw_arrow = false;
                continue;
            }
        }

        // Track arrow => which starts a function scope
        if (c == '=' and i + 1 < expr.len and expr[i + 1] == '>') {
            saw_arrow = true;
            i += 2;
            continue;
        }

        // Track function body entry
        if (c == '{') {
            if (saw_arrow) {
                // Opening brace after => starts function body
                func_depth += 1;
                saw_arrow = false;
            } else if (func_depth > 0) {
                // Nested brace inside function body
                func_depth += 1;
            }
            // Note: { at top level without => is an object literal, not a function
            i += 1;
            continue;
        }

        if (c == '}') {
            if (func_depth > 0) {
                func_depth -= 1;
            }
            i += 1;
            saw_arrow = false;
            continue;
        }

        // Non-brace token clears arrow flag (arrow body without braces is a single expression)
        if (saw_arrow and !std.ascii.isWhitespace(c)) {
            saw_arrow = false;
        }

        // Look for const/let/var declarations, but only at top-level scope
        if (std.ascii.isAlphabetic(c)) {
            const kw_start = i;
            while (i < expr.len and std.ascii.isAlphanumeric(expr[i])) : (i += 1) {}
            const keyword = expr[kw_start..i];

            // Track 'function' keyword - the next { will be a function body
            if (std.mem.eql(u8, keyword, "function")) {
                // Skip to opening paren of parameters
                while (i < expr.len and expr[i] != '(') : (i += 1) {}
                if (i < expr.len) {
                    // Skip the parameter list
                    var paren_depth: u32 = 1;
                    i += 1;
                    while (i < expr.len and paren_depth > 0) {
                        if (expr[i] == '(') paren_depth += 1;
                        if (expr[i] == ')') paren_depth -= 1;
                        i += 1;
                    }
                    // Skip whitespace
                    while (i < expr.len and std.ascii.isWhitespace(expr[i])) : (i += 1) {}
                    // The next { is the function body
                    if (i < expr.len and expr[i] == '{') {
                        func_depth += 1;
                        i += 1;
                    }
                }
                continue;
            }

            // Check if this is a declaration keyword - only collect at top-level
            if (func_depth == 0 and
                (std.mem.eql(u8, keyword, "const") or
                    std.mem.eql(u8, keyword, "let") or
                    std.mem.eql(u8, keyword, "var")))
            {
                // Skip whitespace
                while (i < expr.len and std.ascii.isWhitespace(expr[i])) : (i += 1) {}

                // Check for destructuring pattern: { a, b } or [ a, b ]
                if (i < expr.len and (expr[i] == '{' or expr[i] == '[')) {
                    const open_char = expr[i];
                    const close_char: u8 = if (open_char == '{') '}' else ']';
                    const destruct_start = i + 1;
                    i += 1;
                    var depth: u32 = 1;
                    while (i < expr.len and depth > 0) {
                        if (expr[i] == open_char) depth += 1;
                        if (expr[i] == close_char) depth -= 1;
                        if (depth > 0) i += 1;
                    }
                    // Extract bound names from destructuring pattern
                    try extractDestructuredNames(allocator, expr[destruct_start..i], decls);
                    continue;
                }

                // Extract simple identifier
                if (i < expr.len and (std.ascii.isAlphabetic(expr[i]) or expr[i] == '_' or expr[i] == '$')) {
                    const name_start = i;
                    while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '$')) : (i += 1) {}
                    const name = expr[name_start..i];
                    try decls.put(allocator, name, {});
                }
                continue;
            }
            continue;
        }

        i += 1;
    }
}

/// Extracts parameter names from arrow function parentheses content.
/// Handles: a, b, (a: Type), ({ destructured }), etc.
fn extractArrowParamsFromParens(
    allocator: std.mem.Allocator,
    content: []const u8,
    params: *std.StringHashMapUnmanaged(void),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < content.len) {
        // Skip whitespace
        while (i < content.len and std.ascii.isWhitespace(content[i])) : (i += 1) {}
        if (i >= content.len) break;

        // Handle destructuring patterns: { a, b } or [ a, b ]
        if (content[i] == '{' or content[i] == '[') {
            const open_char = content[i];
            const close_char: u8 = if (open_char == '{') '}' else ']';
            const destruct_start = i + 1;
            i += 1;
            var depth: u32 = 1;
            while (i < content.len and depth > 0) {
                if (content[i] == open_char) depth += 1;
                if (content[i] == close_char) depth -= 1;
                if (depth > 0) i += 1;
            }
            const destruct_end = i;
            // Extract bound names from destructuring pattern
            try extractDestructuredNames(allocator, content[destruct_start..destruct_end], params);
            if (i < content.len) i += 1; // Skip closing brace
            // Skip type annotation if present (: Type)
            while (i < content.len and content[i] != ',' and content[i] != ')') : (i += 1) {}
            if (i < content.len and content[i] == ',') i += 1;
            continue;
        }

        // Handle rest parameter: ...rest
        if (i + 2 < content.len and content[i] == '.' and content[i + 1] == '.' and content[i + 2] == '.') {
            i += 3;
            // Skip whitespace
            while (i < content.len and std.ascii.isWhitespace(content[i])) : (i += 1) {}
        }

        // Extract identifier
        if (i < content.len and (std.ascii.isAlphabetic(content[i]) or content[i] == '_' or content[i] == '$')) {
            const name_start = i;
            while (i < content.len and (std.ascii.isAlphanumeric(content[i]) or content[i] == '_' or content[i] == '$')) : (i += 1) {}
            const name = content[name_start..i];
            try params.put(allocator, name, {});
        }

        // Skip type annotation and default value until comma or end
        while (i < content.len and content[i] != ',') {
            // Skip nested generics in type annotations
            if (content[i] == '<') {
                var depth: u32 = 1;
                i += 1;
                while (i < content.len and depth > 0) {
                    if (content[i] == '<') depth += 1;
                    if (content[i] == '>') depth -= 1;
                    i += 1;
                }
                continue;
            }
            i += 1;
        }
        if (i < content.len and content[i] == ',') i += 1;
    }
}

/// Extracts bound variable names from object/array destructuring patterns.
/// For `{ a, b: c, d: { e } }` extracts: a, c, e (the bound names, not property keys)
/// For `[ a, b ]` extracts: a, b
fn extractDestructuredNames(
    allocator: std.mem.Allocator,
    pattern: []const u8,
    params: *std.StringHashMapUnmanaged(void),
) std.mem.Allocator.Error!void {
    var i: usize = 0;
    while (i < pattern.len) {
        // Skip whitespace
        while (i < pattern.len and std.ascii.isWhitespace(pattern[i])) : (i += 1) {}
        if (i >= pattern.len) break;

        // Handle nested destructuring
        if (pattern[i] == '{' or pattern[i] == '[') {
            const open_char = pattern[i];
            const close_char: u8 = if (open_char == '{') '}' else ']';
            const nested_start = i + 1;
            i += 1;
            var depth: u32 = 1;
            while (i < pattern.len and depth > 0) {
                if (pattern[i] == open_char) depth += 1;
                if (pattern[i] == close_char) depth -= 1;
                if (depth > 0) i += 1;
            }
            const nested_end = i;
            // Recursively extract from nested pattern
            try extractDestructuredNames(allocator, pattern[nested_start..nested_end], params);
            if (i < pattern.len) i += 1; // Skip closing brace
            // Skip any trailing content (type annotation, default value) until comma
            while (i < pattern.len and pattern[i] != ',') : (i += 1) {}
            if (i < pattern.len and pattern[i] == ',') i += 1;
            continue;
        }

        // Handle rest pattern: ...rest
        if (i + 2 < pattern.len and pattern[i] == '.' and pattern[i + 1] == '.' and pattern[i + 2] == '.') {
            i += 3;
            while (i < pattern.len and std.ascii.isWhitespace(pattern[i])) : (i += 1) {}
            // Extract the rest identifier
            if (i < pattern.len and (std.ascii.isAlphabetic(pattern[i]) or pattern[i] == '_' or pattern[i] == '$')) {
                const name_start = i;
                while (i < pattern.len and (std.ascii.isAlphanumeric(pattern[i]) or pattern[i] == '_' or pattern[i] == '$')) : (i += 1) {}
                try params.put(allocator, pattern[name_start..i], {});
            }
            continue;
        }

        // Extract identifier (property name or simple binding)
        if (std.ascii.isAlphabetic(pattern[i]) or pattern[i] == '_' or pattern[i] == '$') {
            const ident_start = i;
            while (i < pattern.len and (std.ascii.isAlphanumeric(pattern[i]) or pattern[i] == '_' or pattern[i] == '$')) : (i += 1) {}
            const ident = pattern[ident_start..i];

            // Skip whitespace after identifier
            while (i < pattern.len and std.ascii.isWhitespace(pattern[i])) : (i += 1) {}

            // Check if this is a rename pattern: key: alias or key: { nested }
            if (i < pattern.len and pattern[i] == ':') {
                i += 1; // Skip colon
                while (i < pattern.len and std.ascii.isWhitespace(pattern[i])) : (i += 1) {}

                // Check for nested destructuring after colon
                if (i < pattern.len and (pattern[i] == '{' or pattern[i] == '[')) {
                    // The identifier before : is the property key, not a binding
                    // Recursively handle the nested pattern
                    continue;
                }

                // Extract the alias (bound name)
                if (i < pattern.len and (std.ascii.isAlphabetic(pattern[i]) or pattern[i] == '_' or pattern[i] == '$')) {
                    const alias_start = i;
                    while (i < pattern.len and (std.ascii.isAlphanumeric(pattern[i]) or pattern[i] == '_' or pattern[i] == '$')) : (i += 1) {}
                    try params.put(allocator, pattern[alias_start..i], {});
                }
            } else {
                // Simple binding (no colon), the identifier itself is the bound name
                try params.put(allocator, ident, {});
            }

            // Skip default value (= ...) and type annotation (: Type) until comma
            while (i < pattern.len and pattern[i] != ',') {
                // Skip nested braces/brackets in default values
                if (pattern[i] == '{' or pattern[i] == '[' or pattern[i] == '(') {
                    const open = pattern[i];
                    const close: u8 = switch (open) {
                        '{' => '}',
                        '[' => ']',
                        '(' => ')',
                        else => unreachable,
                    };
                    i += 1;
                    var depth: u32 = 1;
                    while (i < pattern.len and depth > 0) {
                        if (pattern[i] == open) depth += 1;
                        if (pattern[i] == close) depth -= 1;
                        i += 1;
                    }
                    continue;
                }
                // Skip strings
                if (pattern[i] == '"' or pattern[i] == '\'' or pattern[i] == '`') {
                    i = skipStringLiteral(pattern, i);
                    continue;
                }
                i += 1;
            }
            if (i < pattern.len and pattern[i] == ',') i += 1;
            continue;
        }

        // Skip any other characters (commas, etc.)
        i += 1;
    }
}

/// Skips a string literal starting at position i, returning position after closing quote.
/// For template literals, also extracts identifiers from interpolations.
fn skipStringLiteralAndExtract(
    allocator: std.mem.Allocator,
    expr: []const u8,
    start: usize,
    base_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
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
            const interp_offset = base_offset + @as(u32, @intCast(interp_start));
            try extractIdentifiersFromExpr(allocator, expr[interp_start..interp_end], interp_offset, refs);
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

/// Context-aware version that passes exclusion sets to recursive calls.
/// This ensures local variables declared in outer scopes are excluded from interpolations.
fn skipStringLiteralAndExtractWithContext(
    allocator: std.mem.Allocator,
    expr: []const u8,
    start: usize,
    base_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
    arrow_params: *const std.StringHashMapUnmanaged(void),
    local_decls: *const std.StringHashMapUnmanaged(void),
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
            // Recursively extract identifiers, passing along exclusion context
            const interp_offset = base_offset + @as(u32, @intCast(interp_start));
            try extractIdentifiersFromExprWithContext(allocator, expr[interp_start..interp_end], interp_offset, refs, arrow_params, local_decls, &.{});
            if (i < expr.len) i += 1; // skip closing }
            continue;
        }
        i += 1;
    }
    return i;
}

/// Skips a TypeScript return type annotation in an arrow function.
/// Handles type predicates like `x is Type`, generics like `Promise<T>`, etc.
/// Returns the position just before `=>`.
fn skipReturnTypeAnnotation(expr: []const u8, start: usize) usize {
    if (start >= expr.len or expr[start] != ':') return start;
    var j = start + 1;
    var angle_depth: u32 = 0;
    while (j < expr.len) {
        if (expr[j] == '<') angle_depth += 1;
        if (expr[j] == '>' and angle_depth > 0) angle_depth -= 1;
        if (angle_depth == 0 and j + 1 < expr.len and expr[j] == '=' and expr[j + 1] == '>') {
            return j;
        }
        j += 1;
    }
    return j;
}

/// Heuristic to determine if a `/` is the start of a regex literal.
/// Regex can appear after operators, punctuation, or keywords that expect an expression.
/// This is a conservative check - may have false negatives but avoids false positives.
fn couldBeRegex(expr: []const u8, pos: usize) bool {
    if (pos == 0) return true; // At start, must be regex

    // Look backward, skipping whitespace
    var i = pos;
    while (i > 0) {
        i -= 1;
        const c = expr[i];
        if (std.ascii.isWhitespace(c)) continue;

        // Regex follows these tokens (operators, punctuation, keywords)
        return switch (c) {
            '(', ',', '=', ':', '[', '!', '&', '|', '?', '{', ';', '\n' => true,
            // Could also be after keywords like `return`, `if`, `while`, etc.
            // But checking those is complex - be conservative
            else => false,
        };
    }
    return true; // At start
}

/// Skips a regex literal /pattern/flags.
/// Returns position after the regex (after closing / and optional flags).
fn skipRegexLiteral(expr: []const u8, start: usize) usize {
    if (start >= expr.len or expr[start] != '/') return start;
    var i = start + 1;

    // Find closing /
    while (i < expr.len) {
        const c = expr[i];
        if (c == '\\' and i + 1 < expr.len) {
            i += 2; // Skip escaped char
            continue;
        }
        if (c == '/') {
            i += 1;
            // Skip optional flags (gimsuvy)
            while (i < expr.len and std.ascii.isAlphabetic(expr[i])) : (i += 1) {}
            return i;
        }
        if (c == '\n') {
            // Regex can't span lines - this isn't a regex
            return start + 1;
        }
        i += 1;
    }
    return i;
}

/// Checks if an identifier is a JavaScript keyword that shouldn't be extracted as a reference.
fn isJavaScriptKeyword(ident: []const u8) bool {
    const keywords = [_][]const u8{
        "true",      "false",    "null",      "undefined",  "NaN",       "Infinity",
        "this",      "super",    "arguments", "if",         "else",      "for",
        "while",     "do",       "switch",    "case",       "default",   "break",
        "continue",  "return",   "try",       "catch",      "finally",   "throw",
        "new",       "delete",   "typeof",    "instanceof", "in",        "of",
        "function",  "class",    "extends",   "static",     "get",       "set",
        "const",     "let",      "var",       "async",      "await",     "yield",
        "import",    "export",   "from",      "as",         "void",      "with",
        "debugger",
        // TypeScript keywords
         "readonly", "keyof",     "type",       "interface", "enum",
        "namespace", "module",   "declare",   "abstract",   "private",   "protected",
        "public",    "override", "infer",     "never",      "unknown",   "any",
        "is",        "asserts",  "satisfies",
    };
    for (keywords) |kw| {
        if (std.mem.eql(u8, ident, kw)) return true;
    }
    return false;
}

/// Check if an identifier at position `ident_start` is in an object literal key context.
/// Returns true if the identifier appears to be a property key (e.g., `{ key: value }`).
/// Looks backward for `{` or `,` that would indicate object literal context.
/// Skips string literals and nested structures when searching backward.
fn isObjectLiteralKeyContext(expr: []const u8, ident_start: usize) bool {
    if (ident_start == 0) return false;

    // Scan backward, skipping whitespace, to find what precedes this identifier
    var i = ident_start;
    while (i > 0) {
        i -= 1;
        const c = expr[i];

        // Skip whitespace
        if (std.ascii.isWhitespace(c)) continue;

        // Found { or , - this is an object literal context
        if (c == '{' or c == ',') {
            // But NOT if preceded by $ (like ${ in template literal)
            if (c == '{' and i > 0 and expr[i - 1] == '$') {
                return false;
            }
            return true;
        }

        // Found [ - this could be array literal or computed property, not object key
        if (c == '[') return false;

        // Found ( - not in object literal context (function params)
        if (c == '(') return false;

        // Found : - we're after a value, not before a key
        if (c == ':') return false;

        // Found ? - could be ternary or optional, check context
        if (c == '?') return false;

        // Check if we're in a single-line comment: scan this line for //
        // If the current position is after a // on the same line, skip to before //
        if (skipLineCommentBackward(expr, i)) |new_i| {
            i = new_i;
            continue;
        }

        // Found identifier char - there's something before us, not a fresh key position
        if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') return false;

        // Found closing bracket - scan past the matched opener
        if (c == ')' or c == ']' or c == '}') {
            // Skip backward past the matching opening bracket
            const open_char: u8 = switch (c) {
                ')' => '(',
                ']' => '[',
                '}' => '{',
                else => unreachable,
            };
            var depth: u32 = 1;
            while (i > 0 and depth > 0) {
                i -= 1;
                if (expr[i] == c) depth += 1;
                if (expr[i] == open_char) depth -= 1;
            }
            // After skipping the block, continue scanning backward
            continue;
        }

        // Found string end quote - not in key position (value context)
        if (c == '"' or c == '\'' or c == '`') return false;

        // Other characters - probably not in key position
        return false;
    }

    return false;
}

/// Checks if position `i` is inside a single-line comment on the same line.
/// If so, returns the position just before the // to continue scanning.
/// Returns null if not in a comment.
fn skipLineCommentBackward(expr: []const u8, i: usize) ?usize {
    // Find the start of this line (previous newline or start of string)
    var line_start: usize = 0;
    var j = i;
    while (j > 0) {
        j -= 1;
        if (expr[j] == '\n') {
            line_start = j + 1;
            break;
        }
    }

    // Scan forward from line start looking for // that's not in a string
    var k = line_start;
    while (k + 1 < expr.len and k < i) {
        const c = expr[k];
        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            k += 1;
            while (k < expr.len and k <= i) {
                if (expr[k] == '\\' and k + 1 < expr.len) {
                    k += 2;
                    continue;
                }
                if (expr[k] == c) {
                    k += 1;
                    break;
                }
                k += 1;
            }
            continue;
        }
        // Check for //
        if (c == '/' and expr[k + 1] == '/') {
            // i is after //, so we're in a comment
            if (i > k) {
                // Return position before the //, or 0 if at start
                return if (k > 0) k - 1 else 0;
            }
        }
        k += 1;
    }
    return null;
}

/// Finds the position after the matching closing brace, starting from the position
/// after an opening brace. Properly handles nested braces and skips over string
/// literals (single, double, and template) to avoid counting braces inside strings.
fn findMatchingCloseBrace(template: []const u8, start: usize) usize {
    var depth: u32 = 1;
    var i = start;
    while (i < template.len and depth > 0) {
        const c = template[i];
        // Skip line comments - apostrophes inside shouldn't be treated as strings
        if (c == '/' and i + 1 < template.len and template[i + 1] == '/') {
            i += 2;
            while (i < template.len and template[i] != '\n') : (i += 1) {}
            continue;
        }
        // Skip block comments
        if (c == '/' and i + 1 < template.len and template[i + 1] == '*') {
            i += 2;
            while (i + 1 < template.len) {
                if (template[i] == '*' and template[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }
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
    mappings: *std.ArrayList(SourceMap.Mapping),
    template_refs: *std.StringHashMapUnmanaged(u32),
    has_expressions: *bool,
    is_typescript: bool,
    snippet_body_ranges: []const SnippetBodyRange,
    if_branches: []const IfBranch,
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
        // Skip HTML comments <!-- ... --> to avoid extracting {expr} from documentation
        if (template[i] == '<' and i + 3 < template.len and
            template[i + 1] == '!' and template[i + 2] == '-' and template[i + 3] == '-')
        {
            i += 4;
            while (i + 2 < template.len) {
                if (template[i] == '-' and template[i + 1] == '-' and template[i + 2] == '>') {
                    i += 3;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // Skip <style> blocks to avoid extracting identifiers from CSS
        // This catches <style> inside <svelte:head> which may not be at column 0
        if (template[i] == '<' and std.mem.startsWith(u8, template[i..], "<style")) {
            // Find matching </style>
            if (std.mem.indexOf(u8, template[i..], "</style>")) |end_offset| {
                i = i + end_offset + 8; // Skip past </style>
                continue;
            }
            // No closing tag found, skip to end
            break;
        }

        if (template[i] != '{') {
            i += 1;
            continue;
        }

        const expr_start = i;

        // Find matching closing brace, respecting nesting and string literals
        const j = findMatchingCloseBrace(template, i + 1);

        const full_expr = template[expr_start..j];

        // Skip snippet blocks - they're handled separately and contain type annotations
        // that would be incorrectly extracted as identifiers (e.g., { bracket: "<" })
        if (std.mem.startsWith(u8, full_expr, "{#snippet ") or
            std.mem.startsWith(u8, full_expr, "{/snippet"))
        {
            i = j;
            continue;
        }

        // Svelte 5 attach directives: {@attach functionName(args)}
        // Extract identifiers from the expression to mark the function as "used"
        if (std.mem.startsWith(u8, full_expr, "{@attach ")) {
            const svelte_offset: u32 = @intCast(template_start + expr_start);
            try extractIdentifiersFromExpr(allocator, full_expr, svelte_offset, template_refs);
            i = j;
            continue;
        }

        // Extract identifiers from the expression
        // template_start + expr_start gives the absolute Svelte source offset
        const svelte_offset: u32 = @intCast(template_start + expr_start);
        try extractIdentifiersFromExpr(allocator, full_expr, svelte_offset, template_refs);

        // Special handling for {#each} - emit binding declarations with narrowing
        // Only emit if AST didn't already capture this {#each} block
        if (std.mem.startsWith(u8, full_expr, "{#each ")) {
            const abs_start = svelte_offset;
            const abs_end: u32 = @intCast(template_start + j);

            // Check if this {#each} was already handled by AST-based emission
            var already_handled = false;
            for (ast.nodes.items) |node| {
                if (node.kind == .each_block and node.start == abs_start) {
                    already_handled = true;
                    break;
                }
            }

            if (!already_handled) {
                if (extractEachBindings(ast.source, abs_start, abs_end)) |binding| {
                    if (!has_expressions.*) {
                        try output.appendSlice(allocator, ";// Template expressions\n");
                        has_expressions.* = true;
                    }
                    // If inside snippet body, emit a simplified declaration without referencing
                    // the iterable (which may be a snippet parameter not in scope at module level)
                    if (isInsideSnippetBody(abs_start, snippet_body_ranges)) {
                        try emitEachBindingDeclarationsSimplified(allocator, output, binding, is_typescript);
                    } else {
                        // Use narrowed emission so {#each} inside {#if} respects type narrowing
                        try emitNarrowedEachBindingDeclarations(allocator, output, mappings, binding, is_typescript, if_branches);
                    }
                }
            }
        }

        // Special handling for {@const} inside snippet bodies only.
        // Non-snippet {@const} is handled by emitTemplateExpressions via AST nodes.
        // For snippet bodies, we need to emit here with `any` type since we skip
        // snippet body nodes in emitTemplateExpressions.
        if (std.mem.startsWith(u8, full_expr, "{@const ") and isInsideSnippetBody(svelte_offset, snippet_body_ranges)) {
            if (extractConstBinding(template, @intCast(expr_start), @intCast(j))) |binding| {
                if (!has_expressions.*) {
                    try output.appendSlice(allocator, ";// Template expressions\n");
                    has_expressions.* = true;
                }
                // Use any type to avoid referencing snippet params.
                // Strip any existing type annotation from the name (e.g., "x: number" -> "x")
                const name_without_type = if (std.mem.indexOf(u8, binding.name, ":")) |colon_pos|
                    std.mem.trim(u8, binding.name[0..colon_pos], " \t\n\r")
                else
                    binding.name;
                try output.appendSlice(allocator, "var ");
                try output.appendSlice(allocator, name_without_type);
                if (is_typescript) {
                    try output.appendSlice(allocator, ": any;\n");
                } else {
                    try output.appendSlice(allocator, ";\n");
                }
            }
        }

        i = j; // move past this expression
    }

    // Scan for component tags (capitalized tag names)
    try scanTemplateForComponents(allocator, template, @intCast(template_start), template_refs);
}

/// Scans template source for component usages (capitalized tag names) and
/// attribute expressions. Fallback for when the parser doesn't detect elements
/// inside block structures.
fn scanTemplateForComponents(
    allocator: std.mem.Allocator,
    template: []const u8,
    template_start_offset: u32,
    refs: *std.StringHashMapUnmanaged(u32),
) !void {
    var i: usize = 0;
    while (i < template.len) {
        // Skip {#snippet ...} blocks - they contain TypeScript generics like <T extends ...>
        // that would be misinterpreted as component tags
        if (template[i] == '{' and i + 1 < template.len and template[i + 1] == '#') {
            // Check if this is a snippet block
            if (std.mem.startsWith(u8, template[i..], "{#snippet ")) {
                // Skip to matching closing brace
                i = findMatchingCloseBrace(template, i + 1);
                continue;
            }
        }

        if (template[i] == '<' and i + 1 < template.len) {
            const next_char = template[i + 1];
            // Skip closing tags and special tags
            if (next_char == '/' or next_char == '?') {
                i += 1;
                continue;
            }

            // Skip HTML comments <!-- ... -->
            if (next_char == '!' and i + 3 < template.len and template[i + 2] == '-' and template[i + 3] == '-') {
                // Find end of comment -->
                i += 4;
                while (i + 2 < template.len) {
                    if (template[i] == '-' and template[i + 1] == '-' and template[i + 2] == '>') {
                        i += 3;
                        break;
                    }
                    i += 1;
                }
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
                        const result = try refs.getOrPut(allocator, name);
                        if (!result.found_existing) {
                            result.value_ptr.* = template_start_offset + @as(u32, @intCast(i + 1));
                        }
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
                        // Skip @attach directives - they're handled separately
                        const attr_expr = template[expr_start..i];
                        if (!std.mem.startsWith(u8, attr_expr, "{@attach ")) {
                            const svelte_offset = template_start_offset + @as(u32, @intCast(expr_start));
                            try extractIdentifiersFromExpr(allocator, attr_expr, svelte_offset, refs);
                        }
                    } else if (template[i] == '/' and i + 1 < template.len and template[i + 1] == '>') {
                        i += 2; // Skip />
                        break;
                    } else if (std.mem.startsWith(u8, template[i..], "bind:")) {
                        // Detect bind:xxx shorthand (bind:open means bind:open={open})
                        const bind_offset = template_start_offset + @as(u32, @intCast(i));
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
                                const result = try refs.getOrPut(allocator, binding_name);
                                if (!result.found_existing) {
                                    result.value_ptr.* = bind_offset;
                                }
                            }
                            // If there's =, the { } will be caught by the expression handler above
                        }
                    } else if (detectDirectiveIdentifier(template, i)) |directive_info| {
                        // Detect Svelte directives that reference identifiers:
                        // transition:fade, in:fly, out:slide, animate:flip, use:action
                        const result = try refs.getOrPut(allocator, directive_info.name);
                        if (!result.found_existing) {
                            result.value_ptr.* = template_start_offset + @as(u32, @intCast(i));
                        }
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
/// Note: Must check for word boundary before matching short prefixes like "in:" and "out:"
/// to avoid false matches in CSS like "focus-within:inset" or "fade-out:slide".
fn detectDirectiveIdentifier(template: []const u8, pos: usize) ?DirectiveInfo {
    const directives = [_][]const u8{ "transition:", "animate:", "use:", "in:", "out:" };

    for (directives) |prefix| {
        if (pos + prefix.len < template.len and std.mem.startsWith(u8, template[pos..], prefix)) {
            // For short prefixes like "in:" and "out:", require word boundary before
            // to avoid matching CSS like "focus-within:inset" or "fade-out:slide"
            if ((std.mem.eql(u8, prefix, "in:") or std.mem.eql(u8, prefix, "out:")) and pos > 0) {
                const prev = template[pos - 1];
                // Must be preceded by whitespace or = (attribute start) or " (quoted value start)
                // Not an alphanumeric char or hyphen (which would indicate CSS)
                if (std.ascii.isAlphanumeric(prev) or prev == '-' or prev == '_') {
                    continue;
                }
            }
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

/// Returns true if a component prop should be validated against the component's props type.
/// Excludes Svelte-specific directives that have special handling.
fn isValidatableComponentProp(name: []const u8) bool {
    // Skip Svelte directives (colon syntax)
    if (std.mem.startsWith(u8, name, "bind:")) return false;
    if (std.mem.startsWith(u8, name, "on:")) return false;
    if (std.mem.startsWith(u8, name, "use:")) return false;
    if (std.mem.startsWith(u8, name, "let:")) return false;
    if (std.mem.startsWith(u8, name, "class:")) return false;
    if (std.mem.startsWith(u8, name, "style:")) return false;
    if (std.mem.startsWith(u8, name, "transition:")) return false;
    if (std.mem.startsWith(u8, name, "in:")) return false;
    if (std.mem.startsWith(u8, name, "out:")) return false;
    if (std.mem.startsWith(u8, name, "animate:")) return false;
    // Skip Svelte 5 attach directive
    if (std.mem.eql(u8, name, "@attach")) return false;
    // Skip spread attributes
    if (std.mem.eql(u8, name, "...")) return false;
    // Skip slot attribute (for named slots)
    if (std.mem.eql(u8, name, "slot")) return false;
    // Skip this attribute (for svelte:component)
    if (std.mem.eql(u8, name, "this")) return false;
    return true;
}

/// Returns true if an HTML attribute name should be validated against Svelte's type definitions.
/// Excludes Svelte-specific directives that have special handling.
fn isValidatableHtmlAttr(name: []const u8) bool {
    // Skip Svelte directives
    if (std.mem.startsWith(u8, name, "on:")) return false;
    if (std.mem.startsWith(u8, name, "bind:")) return false;
    if (std.mem.startsWith(u8, name, "use:")) return false;
    if (std.mem.startsWith(u8, name, "class:")) return false;
    if (std.mem.startsWith(u8, name, "style:")) return false;
    if (std.mem.startsWith(u8, name, "transition:")) return false;
    if (std.mem.startsWith(u8, name, "in:")) return false;
    if (std.mem.startsWith(u8, name, "out:")) return false;
    if (std.mem.startsWith(u8, name, "animate:")) return false;
    if (std.mem.startsWith(u8, name, "let:")) return false;
    // Skip Svelte 5 attach directive
    if (std.mem.eql(u8, name, "@attach")) return false;
    // Skip spread attributes
    if (std.mem.eql(u8, name, "...")) return false;
    // Skip slot attribute (for named slots)
    if (std.mem.eql(u8, name, "slot")) return false;
    // Skip this attribute (for svelte:component)
    if (std.mem.eql(u8, name, "this")) return false;
    return true;
}

/// Returns true if an HTML attribute should be typed as a number.
/// These attributes are commonly written as strings in HTML (e.g., tabindex="0")
/// but TypeScript type definitions expect numeric values.
fn isNumericHtmlAttr(name: []const u8) bool {
    // tabindex is the most common numeric attribute
    if (std.mem.eql(u8, name, "tabindex")) return true;
    // aria-level, aria-posinset, aria-setsize, aria-colcount, aria-rowcount, etc.
    if (std.mem.startsWith(u8, name, "aria-")) {
        const aria_name = name["aria-".len..];
        const numeric_aria = [_][]const u8{
            "level",    "posinset",     "setsize",      "colcount",
            "rowcount", "colindex",     "rowindex",     "colspan",
            "rowspan",  "colindextext", "rowindextext", "valuemax",
            "valuemin", "valuenow",
        };
        for (numeric_aria) |attr| {
            if (std.mem.eql(u8, aria_name, attr)) return true;
        }
    }
    // Form-related numeric attributes
    const numeric_attrs = [_][]const u8{
        "maxlength", "minlength", "size",    "cols",    "rows",
        "height",    "width",     "start",   "step",    "max",
        "min",       "span",      "colspan", "rowspan",
    };
    for (numeric_attrs) |attr| {
        if (std.mem.eql(u8, name, attr)) return true;
    }
    return false;
}

/// Returns true if a string value looks like a valid integer (possibly negative).
fn isIntegerString(value: []const u8) bool {
    if (value.len == 0) return false;
    var i: usize = 0;
    // Allow leading minus sign
    if (value[0] == '-') {
        i = 1;
        if (i >= value.len) return false;
    }
    while (i < value.len) : (i += 1) {
        if (!std.ascii.isDigit(value[i])) return false;
    }
    return true;
}

/// Returns true if the identifier is a JavaScript keyword or built-in.
fn isJsKeywordOrBuiltin(name: []const u8) bool {
    const keywords = [_][]const u8{
        // JS keywords
        "if",        "else",     "for",        "while",     "do",      "switch",
        "case",      "default",  "break",      "continue",  "return",  "throw",
        "try",       "catch",    "finally",    "new",       "delete",  "typeof",
        "void",      "in",       "instanceof", "this",      "class",   "extends",
        "super",     "import",   "export",     "from",      "as",      "async",
        "await",     "yield",    "let",        "const",     "var",     "function",
        "true",      "false",    "null",       "undefined", "NaN",     "Infinity",
        // Reserved keywords (strict mode and future reserved)
        "with",      "enum",     "implements", "interface", "package", "private",
        "protected", "public",   "static",     "satisfies",
        // Built-in objects
        "Array",   "Object",
        "String",    "Number",   "Boolean",    "Symbol",    "BigInt",  "Math",
        "Date",      "RegExp",   "Error",      "JSON",      "Promise", "Map",
        "Set",       "WeakMap",  "WeakSet",    "Proxy",     "Reflect", "console",
        "window",    "document", "globalThis",
        // Svelte keywords in template blocks
        "each",      "then",    "snippet",
        "render",    "html",     "debug",      "key",       "attach",
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
    mappings: *std.ArrayList(SourceMap.Mapping),
    binding: EachBindingInfo,
    iterable: []const u8,
    is_typescript: bool,
) !void {
    // Emit item binding: let item = __svelte_ensureArray(iterable)[0];
    // Using let (not var) prevents TypeScript from merging types across exclusive branches.
    // With var, declarations hoist to function scope and get union types from all branches.
    // With let + incremental narrowing context, each branch keeps its own typed binding.
    // The __svelte_ensureArray validates the iterable is array-like (generating TS errors if not)
    // and handles null/undefined by returning empty array
    // Also transform [...x] spreads inside the iterable to [...(x ?? [])] for null safety
    const safe_iterable = try makeIterableNullSafe(allocator, iterable);

    try output.appendSlice(allocator, "let ");
    try output.appendSlice(allocator, binding.item_binding);
    if (is_typescript) {
        // Use __svelte_ensureArray for type validation in TypeScript
        try output.appendSlice(allocator, " = __svelte_ensureArray(");
        // Add source map for the iterable expression inside __svelte_ensureArray()
        // This maps TS errors on the iterable back to the {#each} block in Svelte source
        try mappings.append(allocator, .{
            .svelte_offset = binding.offset,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(safe_iterable.len),
        });
        try output.appendSlice(allocator, safe_iterable);
        try output.appendSlice(allocator, ")[0];\n");
    } else {
        // For JavaScript, just use simple indexing with null coalescing
        try output.appendSlice(allocator, " = ((");
        try output.appendSlice(allocator, safe_iterable);
        try output.appendSlice(allocator, ") ?? [])[0];\n");
    }

    // Emit index binding if present: let i = 0;
    if (binding.index_binding) |idx| {
        try output.appendSlice(allocator, "let ");
        try output.appendSlice(allocator, idx);
        try output.appendSlice(allocator, " = 0;\n");
    }

    // Emit void statement for key expression to mark it as used and type-checked.
    // This handles cases like {#each items as [key, value] (key)} where key
    // is only used in the key expression, not in the loop body.
    // Source mapping allows TypeScript errors on the key expression (e.g., 'item' is of type 'unknown')
    // to be traced back to the correct Svelte position.
    if (binding.key_expr) |key| {
        try output.appendSlice(allocator, "void (");
        // Add source map for the key expression
        try mappings.append(allocator, .{
            .svelte_offset = binding.key_offset,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(key.len),
        });
        try output.appendSlice(allocator, key);
        try output.appendSlice(allocator, ");\n");
    }
}

/// Emits simplified binding declarations for {#each} blocks inside snippet bodies.
/// Uses `any` type instead of referencing the iterable, since the iterable may be
/// a snippet parameter not in scope at module level.
fn emitEachBindingDeclarationsSimplified(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    binding: EachBindingInfo,
    is_typescript: bool,
) !void {
    // Check if item_binding is a destructuring pattern ({ a, b } or [a, b])
    const is_destructuring = binding.item_binding.len > 0 and
        (binding.item_binding[0] == '{' or binding.item_binding[0] == '[');

    // Emit item binding with any type to avoid referencing snippet parameters
    // For destructuring patterns, use `= undefined as any` to satisfy TypeScript
    try output.appendSlice(allocator, "var ");
    try output.appendSlice(allocator, binding.item_binding);
    if (is_typescript) {
        if (is_destructuring) {
            try output.appendSlice(allocator, " = undefined as any;\n");
        } else {
            try output.appendSlice(allocator, ": any;\n");
        }
    } else {
        try output.appendSlice(allocator, ";\n");
    }

    // Emit index binding if present
    if (binding.index_binding) |idx| {
        try output.appendSlice(allocator, "var ");
        try output.appendSlice(allocator, idx);
        try output.appendSlice(allocator, " = 0;\n");
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
/// Also handles {#each expr} without bindings (generates synthetic item binding)
fn extractEachBindings(source: []const u8, start: u32, end: u32) ?EachBindingInfo {
    const content = source[start..end];
    // Format: {#each expr as item, index (key)} or just {#each expr}
    const prefix = "{#each ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    // Find " as " keyword - may not exist for {#each expr} without bindings
    const as_pos_opt = findAsKeyword(content[expr_start..]);

    var iterable: []const u8 = undefined;
    var item_binding: []const u8 = "$$each_item"; // synthetic binding for no-as case
    var index_binding: ?[]const u8 = null;
    var key_expr: ?[]const u8 = null;
    var binding_end: usize = content.len;

    if (as_pos_opt) |as_pos| {
        iterable = std.mem.trim(u8, content[expr_start .. expr_start + as_pos], " \t\n\r");
        if (iterable.len == 0) return null;

        // Parse bindings after "as"
        const bind_start = expr_start + as_pos + 4; // skip " as "
        binding_end = findEachBindingEnd(content, bind_start);
        const bindings = std.mem.trim(u8, content[bind_start..binding_end], " \t\n\r");
        if (bindings.len == 0) return null;

        // Split on comma to get item and optional index, respecting brackets
        item_binding = bindings;

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
    } else {
        // No "as" keyword - {#each expr} without bindings
        // Find the closing } to get the expression
        const close_pos = std.mem.indexOf(u8, content[expr_start..], "}") orelse return null;
        iterable = std.mem.trim(u8, content[expr_start .. expr_start + close_pos], " \t\n\r");
        if (iterable.len == 0) return null;
        binding_end = expr_start + close_pos;
    }

    // Extract key expression if present: (key) after bindings
    var key_offset: u32 = 0;
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
            // Compute offset of key expression in original source
            // key_start is relative to content, which starts at `start`
            key_offset = start + @as(u32, @intCast(key_start));
        }
    }

    return .{
        .iterable = iterable,
        .item_binding = item_binding,
        .index_binding = index_binding,
        .key_expr = key_expr,
        .key_offset = key_offset,
        .offset = start + @as(u32, @intCast(expr_start)),
    };
}

/// Extracts the binding pattern from {:then value} or {:catch error}
/// keyword is "then" or "catch"
/// Handles simple identifiers: {:then value}
/// And destructuring patterns: {:then { x, y }} or {:then [a, b]}
fn extractThenCatchBinding(source: []const u8, start: u32, end: u32, keyword: []const u8) ?[]const u8 {
    const content = source[start..end];
    // Format: {:then value} or {:catch error} or {:then} (no binding)
    // Or destructuring: {:then { x, y }} or {:then [a, b]}

    // Find the keyword (e.g., ":then" or ":catch")
    var search_pattern: [16]u8 = undefined;
    const pattern = std.fmt.bufPrint(&search_pattern, ":{s}", .{keyword}) catch return null;
    const idx = std.mem.indexOf(u8, content, pattern) orelse return null;

    // Start after the keyword
    var bind_start = idx + pattern.len;

    // Skip whitespace
    while (bind_start < content.len and std.ascii.isWhitespace(content[bind_start])) : (bind_start += 1) {}
    if (bind_start >= content.len) return null;

    // Check for immediate closing brace (no binding)
    if (content[bind_start] == '}') return null;

    // Find the end of the binding pattern
    // This is either:
    // - End of identifier for simple bindings
    // - Matching closing bracket for destructuring patterns
    var bind_end = bind_start;

    if (content[bind_start] == '{') {
        // Object destructuring: find matching }
        var brace_depth: u32 = 0;
        while (bind_end < content.len) {
            const c = content[bind_end];
            if (c == '{') brace_depth += 1;
            if (c == '}') {
                brace_depth -= 1;
                if (brace_depth == 0) {
                    bind_end += 1; // Include the closing }
                    break;
                }
            }
            bind_end += 1;
        }
    } else if (content[bind_start] == '[') {
        // Array destructuring: find matching ]
        var bracket_depth: u32 = 0;
        while (bind_end < content.len) {
            const c = content[bind_end];
            if (c == '[') bracket_depth += 1;
            if (c == ']') {
                bracket_depth -= 1;
                if (bracket_depth == 0) {
                    bind_end += 1; // Include the closing ]
                    break;
                }
            }
            bind_end += 1;
        }
    } else {
        // Simple identifier
        while (bind_end < content.len and (std.ascii.isAlphanumeric(content[bind_end]) or content[bind_end] == '_' or content[bind_end] == '$')) : (bind_end += 1) {}
    }

    if (bind_end == bind_start) return null;
    return std.mem.trim(u8, content[bind_start..bind_end], " \t\n\r");
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

/// Extracts snippet name, generics, and params from {#snippet name<T>(params)}
fn extractSnippetName(source: []const u8, start: u32, end: u32) ?SnippetNameInfo {
    const content = source[start..end];
    // Format: {#snippet name(params)} or {#snippet name<T>(params)}
    const prefix = "{#snippet ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const name_start = idx + prefix.len;

    // Find end of name: stops at '<' (generics), '(' (params), '}' or whitespace
    var name_end = name_start;
    while (name_end < content.len) {
        const c = content[name_end];
        if (c == '<' or c == '(' or c == '}' or std.ascii.isWhitespace(c)) break;
        name_end += 1;
    }

    const name = std.mem.trim(u8, content[name_start..name_end], " \t\n\r");
    if (name.len == 0) return null;

    var pos = name_end;

    // Check for generics: <T extends Foo, U>
    // Must handle string literals inside generics like <T extends { x: "<" } | "<">
    var generics: ?[]const u8 = null;
    if (pos < content.len and content[pos] == '<') {
        const generic_start = pos + 1;
        var angle_depth: u32 = 1;
        pos += 1;
        while (pos < content.len and angle_depth > 0) {
            const c = content[pos];
            // Skip string literals to avoid matching < or > inside strings
            if (c == '"' or c == '\'' or c == '`') {
                pos = skipString(content, pos);
                continue;
            }
            if (c == '<') angle_depth += 1;
            if (c == '>') angle_depth -= 1;
            if (angle_depth > 0) pos += 1;
        }
        const g = std.mem.trim(u8, content[generic_start..pos], " \t\n\r");
        if (g.len > 0) generics = g;
        if (pos < content.len and content[pos] == '>') pos += 1; // Skip closing '>'
    }

    // Check for params
    var params: ?[]const u8 = null;
    if (pos < content.len and content[pos] == '(') {
        const params_start = pos + 1;
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
        .generics = generics,
        .params = params,
        .offset = @intCast(name_start),
    };
}

/// Finds all snippet body ranges in the source.
/// For each {#snippet name(params)}...{/snippet}, returns the body range.
fn findSnippetBodyRanges(
    allocator: std.mem.Allocator,
    source: []const u8,
) !std.ArrayList(SnippetBodyRange) {
    var ranges: std.ArrayList(SnippetBodyRange) = .empty;

    var i: usize = 0;
    while (i < source.len) {
        // Look for {#snippet
        if (i + 9 < source.len and std.mem.eql(u8, source[i .. i + 9], "{#snippet")) {
            const snippet_start = i;

            // Find the closing } of the opening tag
            var j = i + 9;
            var brace_depth: u32 = 1;
            while (j < source.len and brace_depth > 0) {
                const c = source[j];
                if (c == '"' or c == '\'' or c == '`') {
                    j = skipStringLiteral(source, j);
                    continue;
                }
                if (c == '{') brace_depth += 1;
                if (c == '}') brace_depth -= 1;
                if (brace_depth > 0) j += 1;
            }
            const body_start: u32 = @intCast(j + 1); // After the closing }

            // Find matching {/snippet}, handling nested snippets.
            // Count snippet nesting to find the correct closing tag.
            const open_tag = "{#snippet";
            const close_tag = "{/snippet}";
            var snippet_depth: u32 = 1;
            var k = j + 1;
            while (k < source.len and snippet_depth > 0) {
                // Only skip template literals (backticks) to avoid false matches inside ${...}
                // Don't skip single/double quotes because they're often just text content in HTML
                // (e.g., "Owner's Manual" contains an apostrophe, not a string delimiter)
                if (source[k] == '`') {
                    k = skipStringLiteral(source, k);
                    continue;
                }
                // Check for nested {#snippet
                if (k + open_tag.len <= source.len and std.mem.eql(u8, source[k .. k + open_tag.len], open_tag)) {
                    snippet_depth += 1;
                    k += open_tag.len;
                    continue;
                }
                // Check for {/snippet}
                if (k + close_tag.len <= source.len and std.mem.eql(u8, source[k .. k + close_tag.len], close_tag)) {
                    snippet_depth -= 1;
                    if (snippet_depth == 0) {
                        const body_end: u32 = @intCast(k);

                        // Extract snippet info from the opening tag
                        if (extractSnippetName(source, @intCast(snippet_start), body_start)) |info| {
                            try ranges.append(allocator, .{
                                .name = info.name,
                                .params = info.params,
                                .generics = info.generics,
                                .snippet_start = @intCast(snippet_start),
                                .body_start = body_start,
                                .body_end = body_end,
                            });
                        }

                        i = k + close_tag.len;
                        break;
                    }
                    k += close_tag.len;
                    continue;
                }
                k += 1;
            }

            // If we didn't find a matching close tag, advance past the opening
            if (snippet_depth > 0) {
                i = j + 1;
            }
            continue;
        }

        // Only skip template literals (backticks) to avoid false matches inside ${...}
        // Don't skip single/double quotes because they're often just text content in HTML
        if (source[i] == '`') {
            i = skipStringLiteral(source, i);
            continue;
        }

        i += 1;
    }

    return ranges;
}

/// Scans source for {#if}/{:else if}/{:else}/{/if} blocks and builds IfBranch entries.
/// Each branch represents a contiguous region where a condition (or its negation) applies.
/// Nested {#if} blocks are flattened into the list; callers should check all branches.
fn findIfBranches(
    allocator: std.mem.Allocator,
    source: []const u8,
) !std.ArrayList(IfBranch) {
    var branches: std.ArrayList(IfBranch) = .empty;

    // Stack of open if-chains. Each chain tracks:
    // - The current branch's condition and body_start
    // - All prior conditions from this chain (for discriminated union narrowing)
    const StackEntry = struct {
        condition: ?[]const u8,
        body_start: u32,
        is_else: bool,
        // All conditions that must be false for this branch to execute
        prior_conditions: std.ArrayList([]const u8),

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.prior_conditions.deinit(alloc);
        }
    };
    var stack: std.ArrayList(StackEntry) = .empty;
    defer {
        for (stack.items) |*entry| {
            entry.deinit(allocator);
        }
        stack.deinit(allocator);
    }

    var i: usize = 0;
    while (i < source.len) {
        // Only skip template literals (backticks) because they can contain ${...} expressions
        // which might have braces that look like {#if}. We DON'T skip single/double quotes
        // because in Svelte templates, quotes in text content (like "it's") are not JS strings.
        if (source[i] == '`') {
            i = skipStringLiteral(source, i);
            continue;
        }

        // Skip HTML comments
        if (i + 4 <= source.len and std.mem.eql(u8, source[i .. i + 4], "<!--")) {
            const comment_end = std.mem.indexOf(u8, source[i + 4 ..], "-->") orelse {
                i = source.len;
                continue;
            };
            i += 4 + comment_end + 3;
            continue;
        }

        // Check for {#if
        if (i + 4 < source.len and std.mem.eql(u8, source[i .. i + 4], "{#if")) {
            const tag_start = i;
            // Find condition and closing brace
            var j = i + 4;
            var brace_depth: u32 = 1;
            while (j < source.len and brace_depth > 0) {
                const c = source[j];
                if (c == '"' or c == '\'' or c == '`') {
                    j = skipStringLiteral(source, j);
                    continue;
                }
                if (c == '{') brace_depth += 1;
                if (c == '}') brace_depth -= 1;
                if (brace_depth > 0) j += 1;
            }
            const tag_end = j + 1;

            // Extract condition: everything between "{#if " and "}"
            const condition_start = tag_start + 4;
            var cond_end = j;
            // Trim whitespace
            while (cond_end > condition_start and (source[cond_end - 1] == ' ' or source[cond_end - 1] == '\t' or source[cond_end - 1] == '\n' or source[cond_end - 1] == '\r')) {
                cond_end -= 1;
            }
            var cond_start = condition_start;
            while (cond_start < cond_end and (source[cond_start] == ' ' or source[cond_start] == '\t' or source[cond_start] == '\n' or source[cond_start] == '\r')) {
                cond_start += 1;
            }
            const condition = if (cond_start < cond_end) source[cond_start..cond_end] else null;

            // New {#if} starts a fresh chain with no prior conditions
            try stack.append(allocator, .{
                .condition = condition,
                .body_start = @intCast(tag_end),
                .is_else = false,
                .prior_conditions = .empty,
            });

            i = tag_end;
            continue;
        }

        // Check for {:else if or {:else
        if (i + 6 < source.len and std.mem.eql(u8, source[i .. i + 6], "{:else")) {
            if (stack.items.len == 0) {
                i += 6;
                continue;
            }

            const tag_start = i;
            // Find closing brace
            var j = i + 6;
            var brace_depth: u32 = 1;
            while (j < source.len and brace_depth > 0) {
                const c = source[j];
                if (c == '"' or c == '\'' or c == '`') {
                    j = skipStringLiteral(source, j);
                    continue;
                }
                if (c == '{') brace_depth += 1;
                if (c == '}') brace_depth -= 1;
                if (brace_depth > 0) j += 1;
            }
            const tag_end = j + 1;

            // Pop the current branch from the stack
            var prev = stack.pop() orelse {
                i = tag_end;
                continue;
            };

            // Record the completed branch with its prior conditions
            try branches.append(allocator, .{
                .condition = prev.condition,
                .is_else = prev.is_else,
                .body_start = prev.body_start,
                .body_end = @intCast(tag_start),
                .prior_conditions = try allocator.dupe([]const u8, prev.prior_conditions.items),
            });

            // Build prior conditions for the next branch:
            // Include all prior conditions from the completed branch, plus its own condition
            var new_prior_conditions: std.ArrayList([]const u8) = .empty;
            try new_prior_conditions.appendSlice(allocator, prev.prior_conditions.items);
            if (prev.condition) |cond| {
                try new_prior_conditions.append(allocator, cond);
            }
            prev.deinit(allocator);

            // Check if it's {:else if ...} or just {:else}
            const after_else = source[i + 6 .. j];
            const trimmed = std.mem.trim(u8, after_else, " \t\n\r");
            if (std.mem.startsWith(u8, trimmed, "if ") or std.mem.startsWith(u8, trimmed, "if\t") or std.mem.startsWith(u8, trimmed, "if\n")) {
                // {:else if condition}
                const if_start = std.mem.indexOf(u8, trimmed, "if") orelse 0;
                const cond_text = std.mem.trim(u8, trimmed[if_start + 2 ..], " \t\n\r");
                try stack.append(allocator, .{
                    .condition = if (cond_text.len > 0) cond_text else null,
                    .body_start = @intCast(tag_end),
                    .is_else = true,
                    .prior_conditions = new_prior_conditions,
                });
            } else {
                // {:else}
                try stack.append(allocator, .{
                    .condition = null,
                    .body_start = @intCast(tag_end),
                    .is_else = true,
                    .prior_conditions = new_prior_conditions,
                });
            }

            i = tag_end;
            continue;
        }

        // Check for {/if}
        if (i + 5 <= source.len and std.mem.eql(u8, source[i .. i + 5], "{/if}")) {
            if (stack.pop()) |prev_const| {
                var prev = prev_const;
                try branches.append(allocator, .{
                    .condition = prev.condition,
                    .is_else = prev.is_else,
                    .body_start = prev.body_start,
                    .body_end = @intCast(i),
                    .prior_conditions = try allocator.dupe([]const u8, prev.prior_conditions.items),
                });
                prev.deinit(allocator);
            }
            i += 5;
            continue;
        }

        i += 1;
    }

    return branches;
}

/// Scans source for {#each} blocks and returns their body ranges with bindings.
/// Used to determine when expressions need access to loop variables.
fn findEachBodyRanges(
    allocator: std.mem.Allocator,
    source: []const u8,
) !std.ArrayList(EachBodyRange) {
    var ranges: std.ArrayList(EachBodyRange) = .empty;

    // Stack of open each blocks
    const StackEntry = struct {
        iterable: []const u8,
        item_binding: []const u8,
        index_binding: ?[]const u8,
        body_start: u32,
    };
    var stack: std.ArrayList(StackEntry) = .empty;
    defer stack.deinit(allocator);

    var i: usize = 0;
    while (i < source.len) {
        // Skip template literals
        if (source[i] == '`') {
            i = skipStringLiteral(source, i);
            continue;
        }

        // Skip HTML comments
        if (i + 4 <= source.len and std.mem.eql(u8, source[i .. i + 4], "<!--")) {
            const comment_end = std.mem.indexOf(u8, source[i + 4 ..], "-->") orelse {
                i = source.len;
                continue;
            };
            i += 4 + comment_end + 3;
            continue;
        }

        // Check for {#each
        if (i + 6 < source.len and std.mem.eql(u8, source[i .. i + 6], "{#each")) {
            // Find closing brace
            var j = i + 6;
            var brace_depth: u32 = 1;
            while (j < source.len and brace_depth > 0) {
                const c = source[j];
                if (c == '"' or c == '\'' or c == '`') {
                    j = skipStringLiteral(source, j);
                    continue;
                }
                if (c == '{') brace_depth += 1;
                if (c == '}') brace_depth -= 1;
                if (brace_depth > 0) j += 1;
            }
            const tag_end = j + 1;

            // Parse the each block to get bindings
            if (extractEachBindings(source, @intCast(i), @intCast(tag_end))) |binding| {
                try stack.append(allocator, .{
                    .iterable = binding.iterable,
                    .item_binding = binding.item_binding,
                    .index_binding = binding.index_binding,
                    .body_start = @intCast(tag_end),
                });
            }

            i = tag_end;
            continue;
        }

        // Check for {:else} within each - this ends the main body
        // but we still track the whole block until {/each}
        if (i + 6 < source.len and std.mem.eql(u8, source[i .. i + 6], "{:else")) {
            // Find closing brace
            var j = i + 6;
            var brace_depth: u32 = 1;
            while (j < source.len and brace_depth > 0) {
                const c = source[j];
                if (c == '"' or c == '\'' or c == '`') {
                    j = skipStringLiteral(source, j);
                    continue;
                }
                if (c == '{') brace_depth += 1;
                if (c == '}') brace_depth -= 1;
                if (brace_depth > 0) j += 1;
            }
            const tag_end = j + 1;

            // Check if this is an {:else} for an {#each} (vs {#if})
            // by looking at what follows - {:else} for each is just {:else}
            // while {:else if ...} is for {#if}
            const after_else = source[i + 6 .. j];
            const trimmed = std.mem.trim(u8, after_else, " \t\n\r");
            if (trimmed.len == 0 and stack.items.len > 0) {
                // Plain {:else} - close the current each body but keep on stack
                // until we see {/each}
                const entry = stack.pop().?;
                try ranges.append(allocator, .{
                    .iterable = entry.iterable,
                    .item_binding = entry.item_binding,
                    .index_binding = entry.index_binding,
                    .body_start = entry.body_start,
                    .body_end = @intCast(i),
                });
                // Don't push back - the {:else} body doesn't have the bindings
            }

            i = tag_end;
            continue;
        }

        // Check for {/each}
        if (i + 7 <= source.len and std.mem.eql(u8, source[i .. i + 7], "{/each}")) {
            if (stack.pop()) |entry| {
                try ranges.append(allocator, .{
                    .iterable = entry.iterable,
                    .item_binding = entry.item_binding,
                    .index_binding = entry.index_binding,
                    .body_start = entry.body_start,
                    .body_end = @intCast(i),
                });
            }
            i += 7;
            continue;
        }

        i += 1;
    }

    return ranges;
}

/// Finds all EachBodyRanges that contain a given position, from outermost to innermost.
fn findAllEnclosingEachRanges(
    allocator: std.mem.Allocator,
    pos: u32,
    each_ranges: []const EachBodyRange,
) !std.ArrayList(EachBodyRange) {
    var result: std.ArrayList(EachBodyRange) = .empty;

    for (each_ranges) |range| {
        if (pos >= range.body_start and pos < range.body_end) {
            try result.append(allocator, range);
        }
    }

    // Sort by body_start (ascending) to get outermost first
    std.mem.sort(EachBodyRange, result.items, {}, struct {
        fn lessThan(_: void, a: EachBodyRange, b: EachBodyRange) bool {
            return a.body_start < b.body_start;
        }
    }.lessThan);

    return result;
}

/// Emits the {#each} binding declarations for enclosing each blocks.
/// Returns the number of bindings emitted (for closing braces later).
fn emitEachBindingOpeners(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    each_ranges: []const EachBodyRange,
    is_typescript: bool,
) !u32 {
    var count: u32 = 0;
    for (each_ranges) |range| {
        // Emit: let item = __svelte_ensureArray(iterable)[0];
        try output.appendSlice(allocator, " let ");
        try output.appendSlice(allocator, range.item_binding);
        if (is_typescript) {
            // Type annotation to help with destructuring patterns
            try output.appendSlice(allocator, " = __svelte_ensureArray(");
        } else {
            try output.appendSlice(allocator, " = __svelte_ensureArray(");
        }
        try output.appendSlice(allocator, range.iterable);
        try output.appendSlice(allocator, ")[0];");

        // Emit index binding if present
        if (range.index_binding) |idx| {
            try output.appendSlice(allocator, " let ");
            try output.appendSlice(allocator, idx);
            try output.appendSlice(allocator, " = 0;");
        }

        count += 1;
    }
    return count;
}

/// Emits if conditions and each bindings interleaved by their body_start.
/// This ensures proper scoping when {#each} and {#if} are nested.
/// For example: {#each items as item}{#if item.foo}...
/// Must emit: let item = ...; if ((item.foo)) { ... }
/// Returns the number of if-blocks opened (for closing braces later).
fn emitInterleavedConditionsAndBindings(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    if_branches: []const IfBranch,
    each_ranges: []const EachBodyRange,
    is_typescript: bool,
) !u32 {
    // Union type to track both kinds of scopes
    const ScopeItem = union(enum) {
        if_branch: IfBranch,
        each_range: EachBodyRange,

        fn bodyStart(self: @This()) u32 {
            return switch (self) {
                .if_branch => |b| b.body_start,
                .each_range => |r| r.body_start,
            };
        }
    };

    // Collect all items and sort by body_start
    var items: std.ArrayList(ScopeItem) = .empty;
    defer items.deinit(allocator);

    for (if_branches) |branch| {
        try items.append(allocator, .{ .if_branch = branch });
    }
    for (each_ranges) |range| {
        try items.append(allocator, .{ .each_range = range });
    }

    // Sort by body_start (outermost first)
    std.mem.sort(ScopeItem, items.items, {}, struct {
        fn lessThan(_: void, a: ScopeItem, b: ScopeItem) bool {
            return a.bodyStart() < b.bodyStart();
        }
    }.lessThan);

    // Emit items in order
    var if_count: u32 = 0;
    for (items.items) |item| {
        switch (item) {
            .if_branch => |branch| {
                // Emit if condition opener
                try output.appendSlice(allocator, "if (");

                // For {:else} and {:else if} branches, negate prior conditions
                for (branch.prior_conditions) |prior| {
                    try output.appendSlice(allocator, "!(");
                    try output.appendSlice(allocator, prior);
                    try output.appendSlice(allocator, ") && ");
                }

                // Emit the condition (or true for plain {:else})
                if (branch.condition) |cond| {
                    try output.appendSlice(allocator, "(");
                    try output.appendSlice(allocator, cond);
                    try output.appendSlice(allocator, ")");
                } else {
                    try output.appendSlice(allocator, "true");
                }
                try output.appendSlice(allocator, ") {");
                if_count += 1;
            },
            .each_range => |range| {
                // Emit binding declaration (no braces needed)
                try output.appendSlice(allocator, " let ");
                try output.appendSlice(allocator, range.item_binding);
                if (is_typescript) {
                    try output.appendSlice(allocator, " = __svelte_ensureArray(");
                } else {
                    try output.appendSlice(allocator, " = __svelte_ensureArray(");
                }
                try output.appendSlice(allocator, range.iterable);
                try output.appendSlice(allocator, ")[0];");
                // Mark binding as used to suppress "unused variable" warnings.
                // For destructuring patterns like [a, b], this silences the warning.
                try output.appendSlice(allocator, " void ");
                try output.appendSlice(allocator, range.item_binding);
                try output.appendSlice(allocator, ";");

                if (range.index_binding) |idx| {
                    try output.appendSlice(allocator, " let ");
                    try output.appendSlice(allocator, idx);
                    try output.appendSlice(allocator, " = 0;");
                }
            },
        }
    }

    return if_count;
}

/// Finds the IfBranch that contains a given source position.
/// Returns the branch if found, null otherwise.
/// For expressions, we want the innermost branch that contains them.
fn findEnclosingIfBranch(pos: u32, branches: []const IfBranch) ?IfBranch {
    // Find all branches that contain this position
    // Return the one with the smallest range (innermost)
    var best: ?IfBranch = null;
    var best_size: u32 = std.math.maxInt(u32);

    for (branches) |branch| {
        if (pos >= branch.body_start and pos < branch.body_end) {
            const size = branch.body_end - branch.body_start;
            if (size < best_size) {
                best = branch;
                best_size = size;
            }
        }
    }

    return best;
}

/// Finds all IfBranches that contain a given position, from outermost to innermost.
/// This is needed to emit nested if conditions correctly.
fn findAllEnclosingIfBranches(
    allocator: std.mem.Allocator,
    pos: u32,
    branches: []const IfBranch,
) !std.ArrayList(IfBranch) {
    var result: std.ArrayList(IfBranch) = .empty;

    // Collect all branches that contain this position
    for (branches) |branch| {
        if (pos >= branch.body_start and pos < branch.body_end) {
            try result.append(allocator, branch);
        }
    }

    // Sort by body_start (ascending) to get outermost first
    std.mem.sort(IfBranch, result.items, {}, struct {
        fn lessThan(_: void, a: IfBranch, b: IfBranch) bool {
            return a.body_start < b.body_start;
        }
    }.lessThan);

    return result;
}

/// Emits opening if conditions for a list of enclosing branches, including prior condition negations.
/// Returns the number of if blocks opened (caller must close them with `}`).
/// @param indent: Optional indentation prefix to add before each "if ("
fn emitIfConditionOpenersIndented(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    enclosing: []const IfBranch,
    indent: []const u8,
) !u32 {
    var conditions_opened: u32 = 0;

    for (enclosing) |branch| {
        // For {:else} and {:else if} branches, emit negations of prior conditions.
        // All prior conditions must be emitted (not just simple ones) because even
        // compound conditions with || help narrow types. For example, negating
        // `!x || x === 'pending'` gives `x && x !== 'pending'`, which narrows x
        // to exclude falsy values and the 'pending' literal - enabling `in` operator
        // checks like `'user' in x` that require an object type.
        const has_priors = branch.prior_conditions.len > 0;

        if (has_priors or branch.condition != null) {
            try output.appendSlice(allocator, indent);
            try output.appendSlice(allocator, "if (");

            var needs_and = false;

            // Emit negations of all prior conditions
            for (branch.prior_conditions) |prior_cond| {
                if (needs_and) {
                    try output.appendSlice(allocator, " && ");
                }
                try output.appendSlice(allocator, "!(");
                try output.appendSlice(allocator, prior_cond);
                try output.appendSlice(allocator, ")");
                needs_and = true;
            }

            // Emit current condition if present
            if (branch.condition) |cond| {
                if (needs_and) {
                    try output.appendSlice(allocator, " && ");
                }
                try output.appendSlice(allocator, "(");
                try output.appendSlice(allocator, cond);
                try output.appendSlice(allocator, ")");
            }

            try output.appendSlice(allocator, ") { ");
            conditions_opened += 1;
        }
    }

    return conditions_opened;
}

/// Emits opening if conditions for a list of enclosing branches, including prior condition negations.
/// Returns the number of if blocks opened (caller must close them with `}`).
fn emitIfConditionOpeners(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    enclosing: []const IfBranch,
) !u32 {
    return emitIfConditionOpenersIndented(allocator, output, enclosing, "");
}

/// Emits an expression wrapped in the appropriate if conditions for type narrowing.
/// For expressions inside {#if condition} blocks, wraps in `if (condition) { void (expr); }`
/// For expressions inside {:else} branches, also emits negations of prior conditions.
/// For expressions inside {:else if condition} blocks, emits `if (!prior1 && !prior2 && cond) { ... }`
fn emitNarrowedExpression(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
    expr: []const u8,
    svelte_offset: u32,
    if_branches: []const IfBranch,
    has_expressions: *bool,
) !void {
    // Find all enclosing if branches for this expression
    var enclosing = try findAllEnclosingIfBranches(allocator, svelte_offset, if_branches);
    defer enclosing.deinit(allocator);

    if (!has_expressions.*) {
        try output.appendSlice(allocator, ";// Template expressions\n");
        has_expressions.* = true;
    }

    // Emit if wrappers for each enclosing branch
    const conditions_opened = try emitIfConditionOpeners(allocator, output, enclosing.items);

    // Emit the actual expression
    try output.appendSlice(allocator, "void (");
    try mappings.append(allocator, .{
        .svelte_offset = svelte_offset,
        .ts_offset = @intCast(output.items.len),
        .len = @intCast(expr.len),
    });
    try output.appendSlice(allocator, expr);
    try output.appendSlice(allocator, ");");

    // Close all opened if blocks
    var j: u32 = 0;
    while (j < conditions_opened) : (j += 1) {
        try output.appendSlice(allocator, " }");
    }
    try output.appendSlice(allocator, "\n");
}

/// Emits {#each} binding declarations wrapped in the appropriate if conditions for type narrowing.
/// This ensures that {#each data.items as item} inside {#if data} has narrowing applied.
fn emitNarrowedEachBindingDeclarations(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
    binding: EachBindingInfo,
    is_typescript: bool,
    if_branches: []const IfBranch,
) !void {
    // Find all enclosing if branches for this {#each} block
    var enclosing = try findAllEnclosingIfBranches(allocator, binding.offset, if_branches);
    defer enclosing.deinit(allocator);

    // Emit if wrappers for each enclosing branch
    const conditions_opened = try emitIfConditionOpeners(allocator, output, enclosing.items);

    // Emit the binding declarations (using the existing function)
    try emitEachBindingDeclarations(allocator, output, mappings, binding, binding.iterable, is_typescript);

    // Close all opened if blocks
    var j: u32 = 0;
    while (j < conditions_opened) : (j += 1) {
        try output.appendSlice(allocator, "}\n");
    }
}

/// Emits {@const} binding declarations wrapped in the appropriate if conditions for type narrowing.
/// This ensures that {@const x = data.value} inside {#if data} has narrowing applied.
fn emitNarrowedConstBinding(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    binding: ConstBindingInfo,
    svelte_offset: u32,
    if_branches: []const IfBranch,
) !void {
    // Find all enclosing if branches for this {@const}
    var enclosing = try findAllEnclosingIfBranches(allocator, svelte_offset, if_branches);
    defer enclosing.deinit(allocator);

    // Emit if wrappers for each enclosing branch
    const conditions_opened = try emitIfConditionOpeners(allocator, output, enclosing.items);

    // Emit the const binding
    try output.appendSlice(allocator, "var ");
    try output.appendSlice(allocator, binding.name);
    try output.appendSlice(allocator, " = ");
    try output.appendSlice(allocator, binding.expr);
    try output.appendSlice(allocator, ";\n");

    // Close all opened if blocks
    var j: u32 = 0;
    while (j < conditions_opened) : (j += 1) {
        try output.appendSlice(allocator, "}\n");
    }
}

/// Checks if a source position is inside any snippet body.
fn isInsideSnippetBody(pos: u32, ranges: []const SnippetBodyRange) bool {
    for (ranges) |range| {
        if (pos >= range.body_start and pos < range.body_end) {
            return true;
        }
    }
    return false;
}

/// Emits expressions from inside a snippet body, wrapped in a block with snippet parameters in scope.
/// This ensures that expressions like `{@render threadItem(thread)}` inside `{#snippet children(thread: T)}`
/// have access to the `thread` parameter.
/// Also applies if-branch narrowing for expressions inside {#if} blocks within the snippet.
fn emitSnippetBodyExpressions(
    allocator: std.mem.Allocator,
    ast: *const Ast,
    output: *std.ArrayList(u8),
    mappings: *std.ArrayList(SourceMap.Mapping),
    range: SnippetBodyRange,
    is_typescript: bool,
    if_branches: []const IfBranch,
    each_body_ranges: []const EachBodyRange,
    has_expressions: *bool,
) !void {
    // Collect render expressions and other expressions from the snippet body
    var body_exprs: std.ArrayList(struct { expr: []const u8, offset: u32 }) = .empty;
    defer body_exprs.deinit(allocator);

    // Collect components inside the snippet body for props validation
    const ComponentInfo = struct { data_index: u32, node_start: u32 };
    var body_components: std.ArrayList(ComponentInfo) = .empty;
    defer body_components.deinit(allocator);

    // Collect each bindings inside the snippet body
    var each_bindings: std.ArrayList(EachBindingInfo) = .empty;
    defer each_bindings.deinit(allocator);

    for (ast.nodes.items) |node| {
        // Only process nodes within this snippet's body range
        if (node.start < range.body_start or node.start >= range.body_end) continue;

        switch (node.kind) {
            .render => {
                if (extractRenderExpression(ast.source, node.start, node.end)) |render_info| {
                    try body_exprs.append(allocator, .{
                        .expr = render_info.expr,
                        .offset = node.start + render_info.offset,
                    });
                }
            },
            .expression => {
                if (extractPlainExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .if_block => {
                if (extractIfExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .key_block => {
                if (extractKeyExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .html => {
                if (extractHtmlExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .debug_tag => {
                if (extractDebugExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .await_block => {
                if (extractAwaitExpression(ast.source, node.start, node.end)) |expr_info| {
                    try body_exprs.append(allocator, .{
                        .expr = expr_info.expr,
                        .offset = node.start + expr_info.offset,
                    });
                }
            },
            .component => {
                // Collect component for props validation inside snippet scope
                try body_components.append(allocator, .{
                    .data_index = node.data,
                    .node_start = node.start,
                });
            },
            .each_block => {
                // Collect each bindings for proper typing inside snippet scope
                if (extractEachBindings(ast.source, node.start, node.end)) |binding| {
                    try each_bindings.append(allocator, binding);
                }
            },
            else => {},
        }
    }

    // If there are no body expressions, components, or each bindings, nothing to emit
    if (body_exprs.items.len == 0 and body_components.items.len == 0 and each_bindings.items.len == 0) return;

    if (!has_expressions.*) {
        try output.appendSlice(allocator, ";// Template expressions\n");
        has_expressions.* = true;
    }

    // Emit a self-invoking function that provides the snippet parameter scope.
    // Format: ((param1: any, param2: any) => { void (expr1); void (expr2); })(undefined!, undefined!);
    // Using `undefined!` as arguments satisfies TypeScript while making it clear these are placeholders.
    try output.appendSlice(allocator, "((");

    // Emit parameters with proper names and any type
    if (range.params) |params| {
        try emitSnippetParamsWithNames(allocator, output, params, is_typescript);
    }

    try output.appendSlice(allocator, ") => {\n");

    // Find if branches that ENCLOSE the snippet definition itself.
    // These outer conditions must be applied to all expressions inside the snippet
    // for proper discriminated union narrowing.
    var outer_enclosing = try findAllEnclosingIfBranches(allocator, range.snippet_start, if_branches);
    defer outer_enclosing.deinit(allocator);

    // Filter if_branches to only those entirely within this snippet's body range.
    // We only want branches where both body_start and body_end are within the snippet.
    var snippet_branches: std.ArrayList(IfBranch) = .empty;
    defer snippet_branches.deinit(allocator);
    for (if_branches) |branch| {
        if (branch.body_start >= range.body_start and branch.body_end <= range.body_end) {
            try snippet_branches.append(allocator, branch);
        }
    }

    // Find {#each} blocks that ENCLOSE the snippet definition.
    // Expressions inside the snippet need access to these loop variables (e.g., `file` from `{#each files as file}`).
    var enclosing_eaches = try findAllEnclosingEachRanges(allocator, range.snippet_start, each_body_ranges);
    defer enclosing_eaches.deinit(allocator);

    // Emit bindings for enclosing {#each} blocks at the start of the snippet IIFE.
    // This makes loop variables like `file` and `index` available inside the snippet body.
    for (enclosing_eaches.items) |each_range| {
        try output.appendSlice(allocator, "  let ");
        try output.appendSlice(allocator, each_range.item_binding);
        try output.appendSlice(allocator, " = __svelte_ensureArray(");
        try output.appendSlice(allocator, each_range.iterable);
        try output.appendSlice(allocator, ")[0];");
        if (each_range.index_binding) |idx| {
            try output.appendSlice(allocator, " let ");
            try output.appendSlice(allocator, idx);
            try output.appendSlice(allocator, " = 0;");
        }
        try output.appendSlice(allocator, "\n");
    }

    // Emit each bindings with narrowing applied
    // This provides proper typing for loop variables like `{#each m.content as block}`
    for (each_bindings.items) |binding| {
        // Find enclosing if branches for this each block (within snippet scope)
        var inner_enclosing = try findAllEnclosingIfBranches(allocator, binding.offset, snippet_branches.items);
        defer inner_enclosing.deinit(allocator);

        // Emit outer enclosing conditions (from if branches containing the snippet)
        const outer_opened = try emitIfConditionOpenersIndented(allocator, output, outer_enclosing.items, "");
        // Emit inner enclosing conditions (from if branches within the snippet body)
        const inner_opened = try emitIfConditionOpenersIndented(allocator, output, inner_enclosing.items, "");
        const conditions_opened = outer_opened + inner_opened;

        // Emit the each binding declarations
        try emitEachBindingDeclarations(allocator, output, mappings, binding, binding.iterable, is_typescript);

        // Close all opened if blocks
        var j: u32 = 0;
        while (j < conditions_opened) : (j += 1) {
            try output.appendSlice(allocator, "}\n");
        }
    }

    // Emit each body expression with narrowing applied
    for (body_exprs.items) |expr_info| {
        // Find enclosing if branches for this expression (within snippet scope)
        var inner_enclosing = try findAllEnclosingIfBranches(allocator, expr_info.offset, snippet_branches.items);
        defer inner_enclosing.deinit(allocator);

        // Emit outer enclosing conditions (from if branches containing the snippet)
        const outer_opened = try emitIfConditionOpenersIndented(allocator, output, outer_enclosing.items, "  ");
        // Emit inner enclosing conditions (from if branches within the snippet body)
        const inner_opened = try emitIfConditionOpenersIndented(allocator, output, inner_enclosing.items, "  ");
        const conditions_opened = outer_opened + inner_opened;

        // Emit the actual expression
        try output.appendSlice(allocator, "void (");
        try mappings.append(allocator, .{
            .svelte_offset = expr_info.offset,
            .ts_offset = @intCast(output.items.len),
            .len = @intCast(expr_info.expr.len),
        });
        try output.appendSlice(allocator, expr_info.expr);
        try output.appendSlice(allocator, ");");

        // Close all opened if blocks
        var j: u32 = 0;
        while (j < conditions_opened) : (j += 1) {
            try output.appendSlice(allocator, " }");
        }
        try output.appendSlice(allocator, "\n");
    }

    // Emit component props validation for components inside this snippet
    // Only in TypeScript mode (component props validation requires type checking)
    if (is_typescript) {
        for (body_components.items) |comp_info| {
            const elem_data = ast.elements.items[comp_info.data_index];
            const tag_name = elem_data.tag_name;

            // Skip svelte: special elements
            if (std.mem.startsWith(u8, tag_name, "svelte:")) continue;

            // Find enclosing if branches for this component (within snippet scope)
            var inner_enclosing = try findAllEnclosingIfBranches(allocator, comp_info.node_start, snippet_branches.items);
            defer inner_enclosing.deinit(allocator);

            // Emit outer enclosing conditions (from if branches containing the snippet)
            const outer_opened = try emitIfConditionOpenersIndented(allocator, output, outer_enclosing.items, "  ");
            // Emit inner enclosing conditions (from if branches within the snippet body)
            const inner_opened = try emitIfConditionOpenersIndented(allocator, output, inner_enclosing.items, "  ");
            const conditions_opened = outer_opened + inner_opened;

            // Emit component props validation
            try emitComponentPropsValidation(
                allocator,
                ast.source,
                output,
                mappings,
                tag_name,
                elem_data.attrs_start,
                elem_data.attrs_end,
                ast.attributes.items,
            );

            // Close all opened if blocks
            var k: u32 = 0;
            while (k < conditions_opened) : (k += 1) {
                try output.appendSlice(allocator, " }");
            }
            if (conditions_opened > 0) {
                try output.appendSlice(allocator, "\n");
            }
        }
    }

    try output.appendSlice(allocator, "})(");

    // Emit undefined! for each parameter as placeholder arguments
    if (range.params) |params| {
        const param_count = countParams(params);
        var i: usize = 0;
        while (i < param_count) : (i += 1) {
            if (i > 0) try output.appendSlice(allocator, ", ");
            if (is_typescript) {
                try output.appendSlice(allocator, "undefined!");
            } else {
                try output.appendSlice(allocator, "undefined");
            }
        }
    }

    try output.appendSlice(allocator, ");\n");
}

/// Emits snippet parameter declarations with their original names and types.
/// Used for snippet body wrapper functions where we need proper types for narrowing.
/// Preserves type annotations to enable discriminated union narrowing inside snippet bodies.
fn emitSnippetParamsWithNames(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    params: []const u8,
    is_typescript: bool,
) !void {
    _ = is_typescript;
    // Emit the entire params string as-is to preserve type annotations.
    // This enables TypeScript to properly narrow types inside snippet bodies.
    // Example: "(m: ThreadAssistantMessage, messageIndex: number)" preserves
    // the ThreadAssistantMessage type which allows m.content narrowing.
    try output.appendSlice(allocator, params);
}

/// Emits parameter declarations with `any` type for hoisted snippet functions.
/// Counts parameters in `params` string and emits `_0: any, _1: any, ...`.
/// This preserves parameter arity for call-site validation without referencing
/// types that may not be imported yet.
fn emitAnyTypedParams(
    allocator: std.mem.Allocator,
    output: *std.ArrayList(u8),
    params: []const u8,
    is_typescript: bool,
) !void {
    // Parse parameters and emit with proper optionality.
    // We need to preserve whether each param is optional (has ? or = default)
    // so TypeScript correctly validates argument counts at call sites.
    var param_list: std.ArrayList(ParamInfo) = .empty;
    defer param_list.deinit(allocator);
    try parseParams(params, &param_list, allocator);

    for (param_list.items, 0..) |param, idx| {
        if (idx > 0) try output.appendSlice(allocator, ", ");
        try output.appendSlice(allocator, "_");
        var buf: [16]u8 = undefined;
        const idx_str = std.fmt.bufPrint(&buf, "{d}", .{idx}) catch unreachable;
        try output.appendSlice(allocator, idx_str);
        if (is_typescript) {
            if (param.is_optional) {
                try output.appendSlice(allocator, "?: any");
            } else {
                try output.appendSlice(allocator, ": any");
            }
        }
    }
}

const ParamInfo = struct {
    is_optional: bool,
};

/// Parses function parameters and extracts their optionality.
/// Handles: `name`, `name: Type`, `name?: Type`, `name = default`, `name: Type = default`,
/// `{ destructure }: Type`, and trailing commas.
fn parseParams(params: []const u8, list: *std.ArrayList(ParamInfo), allocator: std.mem.Allocator) !void {
    if (params.len == 0) return;

    var i: usize = 0;
    while (i < params.len) {
        // Skip leading whitespace
        while (i < params.len and std.ascii.isWhitespace(params[i])) : (i += 1) {}
        if (i >= params.len) break;

        // Check for trailing comma (no more params after this)
        if (params[i] == ',') {
            i += 1;
            continue;
        }

        // Start of a parameter - find where it ends (next comma at depth 0)
        const param_start = i;
        var depth: u32 = 0;
        var has_question: bool = false;
        var has_equals: bool = false;

        while (i < params.len) {
            const c = params[i];

            // Track nesting depth
            if (c == '{' or c == '[' or c == '(' or c == '<') depth += 1;
            if ((c == '}' or c == ']' or c == ')' or c == '>') and depth > 0) depth -= 1;

            // Skip strings
            if (c == '"' or c == '\'' or c == '`') {
                i = skipString(params, i);
                continue;
            }

            // Track optionality markers at depth 0
            if (depth == 0) {
                // `?` before `:` means optional (like `name?: Type`)
                // We check for `?` followed by `:` to distinguish from `? :` ternary
                if (c == '?' and i + 1 < params.len and params[i + 1] == ':') {
                    has_question = true;
                }
                // `=` means has default value (like `name = value` or `name: Type = value`)
                // Exclude `=>` which is arrow function syntax in types
                if (c == '=' and (i + 1 >= params.len or params[i + 1] != '>')) {
                    has_equals = true;
                }
                // Comma ends the parameter
                if (c == ',') break;
            }

            i += 1;
        }

        // Only add if we actually found content (not just whitespace before comma)
        const param_content = std.mem.trim(u8, params[param_start..i], " \t\n\r");
        if (param_content.len > 0) {
            try list.append(allocator, .{
                .is_optional = has_question or has_equals,
            });
        }

        // Skip the comma
        if (i < params.len and params[i] == ',') i += 1;
    }
}

/// Counts the number of parameters in a params string.
/// Handles destructuring patterns and type annotations.
/// Also used by snippet hoisting to count params for Snippet<[...]> type.
const countSnippetParams = countParams;
fn countParams(params: []const u8) usize {
    if (params.len == 0) return 0;

    var count: usize = 0;
    var i: usize = 0;
    var depth: u32 = 0;
    var in_param: bool = false;

    while (i < params.len) {
        const c = params[i];

        // Track nesting depth for destructuring and generics
        if (c == '{' or c == '[' or c == '(' or c == '<') depth += 1;
        if (c == '}' or c == ']' or c == ')' or c == '>') {
            if (depth > 0) depth -= 1;
        }

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(params, i);
            continue;
        }

        if (depth == 0) {
            // ',' at depth 0 separates parameters
            if (c == ',') {
                in_param = false;
                i += 1;
                continue;
            }

            // Non-whitespace starts a new parameter
            if (!std.ascii.isWhitespace(c) and !in_param) {
                count += 1;
                in_param = true;
            }
        }

        i += 1;
    }

    return count;
}

/// Extracts parameter names from a function parameter string.
/// Handles patterns like "val: T", "x: number, y: string", "{ a, b }: Props", etc.
fn extractParamNames(
    allocator: std.mem.Allocator,
    params: []const u8,
    names: *std.StringHashMapUnmanaged(void),
) !void {
    var i: usize = 0;
    while (i < params.len) {
        // Skip whitespace
        while (i < params.len and std.ascii.isWhitespace(params[i])) : (i += 1) {}
        if (i >= params.len) break;

        // Handle destructuring patterns: { a, b } or [a, b]
        if (params[i] == '{' or params[i] == '[') {
            const open_char = params[i];
            const close_char: u8 = if (open_char == '{') '}' else ']';
            i += 1;
            var depth: u32 = 1;
            while (i < params.len and depth > 0) {
                if (params[i] == open_char) depth += 1;
                if (params[i] == close_char) depth -= 1;
                // Extract identifiers inside the destructuring pattern
                if (depth > 0 and isIdentStartChar(params[i])) {
                    const name_start = i;
                    while (i < params.len and isIdentChar(params[i])) : (i += 1) {}
                    const name = params[name_start..i];
                    // Skip TypeScript keywords
                    if (!std.mem.eql(u8, name, "as") and !std.mem.eql(u8, name, "readonly")) {
                        try names.put(allocator, name, {});
                    }
                } else {
                    i += 1;
                }
            }
            if (i < params.len and params[i] == close_char) i += 1;
            // Skip type annotation after destructuring
            while (i < params.len and params[i] != ',') : (i += 1) {}
            if (i < params.len and params[i] == ',') i += 1;
            continue;
        }

        // Simple parameter: name or name: type
        if (isIdentStartChar(params[i])) {
            const name_start = i;
            while (i < params.len and isIdentChar(params[i])) : (i += 1) {}
            const name = params[name_start..i];
            try names.put(allocator, name, {});
        }

        // Skip to next parameter (past : type annotation and , separator)
        while (i < params.len and params[i] != ',') {
            // Skip nested generics in type annotations
            if (params[i] == '<') {
                var depth: u32 = 1;
                i += 1;
                while (i < params.len and depth > 0) {
                    if (params[i] == '<') depth += 1;
                    if (params[i] == '>') depth -= 1;
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
        if (i < params.len and params[i] == ',') i += 1;
    }
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

/// Extracts the binding pattern from an inline {#await promise then binding} expression.
/// Returns the binding pattern (e.g., "result" or "{ x, y }") or null if not found.
fn extractInlineThenBinding(source: []const u8, start: u32, end: u32) ?[]const u8 {
    const content = source[start..end];
    // Format: {#await expr then binding}
    const prefix = "{#await ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const after_await = content[idx + prefix.len ..];

    // Find " then " keyword
    const then_pos = findThenKeyword(after_await) orelse return null;
    // then_pos points to the space before "then", so skip " then"
    const after_then = after_await[then_pos + 5 ..]; // Skip " then"

    // Skip whitespace after "then"
    var bind_start: usize = 0;
    while (bind_start < after_then.len and std.ascii.isWhitespace(after_then[bind_start])) : (bind_start += 1) {}
    if (bind_start >= after_then.len) return null;

    // Check for immediate closing brace (no binding)
    if (after_then[bind_start] == '}') return null;

    // Find the end of the binding pattern (up to closing brace)
    var bind_end = bind_start;

    if (after_then[bind_start] == '{') {
        // Object destructuring: find matching }
        var brace_depth: u32 = 0;
        while (bind_end < after_then.len) {
            const c = after_then[bind_end];
            if (c == '{') brace_depth += 1;
            if (c == '}') {
                if (brace_depth == 0) break; // End of block (not part of destructuring)
                brace_depth -= 1;
                if (brace_depth == 0) {
                    bind_end += 1; // Include the closing }
                    break;
                }
            }
            bind_end += 1;
        }
    } else if (after_then[bind_start] == '[') {
        // Array destructuring: find matching ]
        var bracket_depth: u32 = 0;
        while (bind_end < after_then.len) {
            const c = after_then[bind_end];
            if (c == '[') bracket_depth += 1;
            if (c == ']') {
                bracket_depth -= 1;
                if (bracket_depth == 0) {
                    bind_end += 1; // Include the closing ]
                    break;
                }
            }
            if (c == '}') break; // End of block
            bind_end += 1;
        }
    } else {
        // Simple identifier
        while (bind_end < after_then.len) {
            const c = after_then[bind_end];
            if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '$') break;
            bind_end += 1;
        }
    }

    if (bind_end == bind_start) return null;
    return std.mem.trim(u8, after_then[bind_start..bind_end], " \t\n\r");
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

/// Extracts expression from plain {expr} template syntax.
/// Returns the expression content without braces.
/// Skips block markers like {/if}, {/each}, {:else}, {@const}, {@debug}, etc.
fn extractPlainExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {expr}
    if (content.len < 2 or content[0] != '{') return null;

    // Skip block end markers ({/if}, {/each}, etc.) and block continuation ({:else}, {:then}, etc.)
    // These are parsed as expression nodes but aren't expressions to type-check
    if (content.len >= 2 and (content[1] == '/' or content[1] == ':' or content[1] == '#' or content[1] == '@')) {
        return null;
    }

    const expr_start: usize = 1;
    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = 1 };
}

/// Extracts expression from {@html expr} template syntax.
fn extractHtmlExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {@html expr}
    const prefix = "{@html ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

    const expr_end = findExpressionEnd(content, expr_start);
    const expr = std.mem.trim(u8, content[expr_start..expr_end], " \t\n\r");
    if (expr.len == 0) return null;
    return .{ .expr = expr, .offset = @intCast(expr_start) };
}

/// Extracts expression from {@debug expr1, expr2, ...} template syntax.
fn extractDebugExpression(source: []const u8, start: u32, end: u32) ?ExprInfo {
    const content = source[start..end];
    // Format: {@debug expr1, expr2, ...}
    const prefix = "{@debug ";
    const idx = std.mem.indexOf(u8, content, prefix) orelse return null;
    const expr_start = idx + prefix.len;

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

/// Convert byte offset to 1-based line and column numbers
fn offsetToLineCol(source: []const u8, offset: usize) struct { line: u32, col: u32 } {
    var line: u32 = 1;
    var col: u32 = 1;
    var i: usize = 0;

    while (i < source.len and i < offset) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
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

/// Strips 'export' keyword from function/const/let/var declarations.
/// Used when script content is wrapped inside a function (for generic components),
/// where 'export' is not valid TypeScript syntax.
/// Patterns transformed:
/// - `export function foo()` → `function foo()`
/// - `export const x = ...` → `const x = ...`
/// - `export let y` → `let y`
/// - `export var z` → `var z`
/// - `export async function f()` → `async function f()`
fn stripExportKeyword(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Early exit: skip if no exports to strip
    if (std.mem.indexOf(u8, content, "export ") == null) return content;

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);
    try result.ensureTotalCapacity(allocator, content.len);

    var i: usize = 0;
    while (i < content.len) {
        // Check for "export " at a word boundary
        if (i + 7 <= content.len and std.mem.eql(u8, content[i .. i + 7], "export ")) {
            // Make sure this is at the start of a line (or start of content)
            const at_line_start = (i == 0) or (i > 0 and content[i - 1] == '\n') or
                (i > 0 and std.ascii.isWhitespace(content[i - 1]) and !isIdentChar(content[i - 1]));

            if (at_line_start) {
                // Check what follows: function, const, let, var, async
                const rest = content[i + 7 ..];
                const stripped = std.mem.trimLeft(u8, rest, " \t");

                const valid_export = std.mem.startsWith(u8, stripped, "function ") or
                    std.mem.startsWith(u8, stripped, "function(") or
                    std.mem.startsWith(u8, stripped, "const ") or
                    std.mem.startsWith(u8, stripped, "let ") or
                    std.mem.startsWith(u8, stripped, "var ") or
                    std.mem.startsWith(u8, stripped, "async ");

                if (valid_export) {
                    // Skip "export " (7 chars)
                    i += 7;
                    continue;
                }
            }
        }

        // Copy character as-is
        try result.append(allocator, content[i]);
        i += 1;
    }

    return try result.toOwnedSlice(allocator);
}

/// Result of separating imports from other code.
const SeparatedCode = struct {
    imports: []const u8,
    other: []const u8,
};

/// Separates import statements from other code.
/// Used for generic components where imports must be at module level but other code
/// goes inside the __render function.
/// Handles:
/// - `import ... from '...'` statements
/// - `import '...'` (side-effect imports)
/// - `import type ... from '...'` (TypeScript type imports)
/// Multi-line imports (with { ... } spanning lines) are supported.
fn separateImports(allocator: std.mem.Allocator, content: []const u8) !SeparatedCode {
    var imports: std.ArrayList(u8) = .empty;
    defer imports.deinit(allocator);
    var other: std.ArrayList(u8) = .empty;
    defer other.deinit(allocator);

    try imports.ensureTotalCapacity(allocator, content.len / 4);
    try other.ensureTotalCapacity(allocator, content.len);

    var i: usize = 0;
    while (i < content.len) {
        // Skip leading whitespace on the line (but preserve it for output)
        const line_start = i;
        while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

        // Check if this line starts with 'import'
        if (i + 6 <= content.len and std.mem.eql(u8, content[i .. i + 6], "import")) {
            // Verify it's actually an import keyword (not identifier like 'importFoo')
            const after_import = if (i + 6 < content.len) content[i + 6] else 0;
            if (after_import == ' ' or after_import == '\t' or after_import == '{' or after_import == '"' or after_import == '\'') {
                // This is an import statement - find where it ends
                const import_end = findImportEnd(content, i);

                // Include the full import (from line_start to include leading whitespace)
                try imports.appendSlice(allocator, content[line_start..import_end]);
                if (import_end < content.len and content[import_end] == '\n') {
                    try imports.append(allocator, '\n');
                    i = import_end + 1;
                } else {
                    i = import_end;
                }
                continue;
            }
        }

        // Not an import - copy to 'other' until end of line
        const line_end = std.mem.indexOfScalarPos(u8, content, i, '\n') orelse content.len;
        try other.appendSlice(allocator, content[line_start..line_end]);
        if (line_end < content.len) {
            try other.append(allocator, '\n');
            i = line_end + 1;
        } else {
            i = line_end;
        }
    }

    return .{
        .imports = try imports.toOwnedSlice(allocator),
        .other = try other.toOwnedSlice(allocator),
    };
}

/// Finds the end of an import statement, handling multi-line imports.
/// Returns the position after the semicolon or newline that ends the import.
fn findImportEnd(content: []const u8, start: usize) usize {
    var i = start;
    var brace_depth: u32 = 0;
    var in_string: u8 = 0;

    while (i < content.len) {
        const c = content[i];

        // Handle string literals
        if (in_string != 0) {
            if (c == '\\' and i + 1 < content.len) {
                i += 2;
                continue;
            }
            if (c == in_string) {
                in_string = 0;
            }
            i += 1;
            continue;
        }

        // Start of string
        if (c == '"' or c == '\'' or c == '`') {
            in_string = c;
            i += 1;
            continue;
        }

        // Track braces for multi-line imports like: import { a, b, c } from '...'
        if (c == '{') {
            brace_depth += 1;
        } else if (c == '}') {
            if (brace_depth > 0) brace_depth -= 1;
        }

        // Semicolon ends the import (if not inside braces)
        if (c == ';' and brace_depth == 0) {
            return i + 1;
        }

        // Newline ends import only if we're not inside braces
        if (c == '\n' and brace_depth == 0) {
            return i;
        }

        i += 1;
    }

    return i;
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

/// Transforms $state with explicit type annotations to preserve the full type.
/// TypeScript's control flow analysis narrows `let x: T = $state(v)` to the type of `v`,
/// which causes false positives like "This comparison appears to be unintentional".
///
/// Transforms: `let activeTab: 'cli' | 'vscode' = $state('cli')`
/// Into:       `let activeTab = $state<'cli' | 'vscode'>('cli')`
///
/// The explicit generic parameter ensures TypeScript uses the full union type.
fn transformStateWithTypeAnnotation(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Early exit: skip if no $state
    if (std.mem.indexOf(u8, content, "$state") == null) return content;

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

        // Look for 'let' or 'const' keywords
        if (startsWithKeyword(content[i..], "let") or startsWithKeyword(content[i..], "const")) {
            const keyword_len: usize = if (startsWithKeyword(content[i..], "const")) 5 else 3;
            const decl_start = i;
            i += keyword_len;

            // Skip whitespace after keyword
            while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

            // Parse identifier
            const name_start = i;
            while (i < content.len and isIdentChar(content[i])) : (i += 1) {}
            const name_end = i;

            if (name_end == name_start) {
                // No identifier found, emit what we have and continue
                try result.appendSlice(allocator, content[decl_start..i]);
                continue;
            }

            // Skip whitespace
            while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

            // Check for type annotation ':'
            if (i < content.len and content[i] == ':') {
                i += 1;
                // Skip whitespace after colon
                while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

                // Parse type annotation (stop at '=' that's not inside generics/parens)
                const type_start = i;
                var angle_depth: u32 = 0;
                var paren_depth: u32 = 0;
                var brace_depth: u32 = 0;
                while (i < content.len) {
                    const c = content[i];
                    if (c == '<') {
                        angle_depth += 1;
                    } else if (c == '>' and angle_depth > 0) {
                        angle_depth -= 1;
                    } else if (c == '(') {
                        paren_depth += 1;
                    } else if (c == ')' and paren_depth > 0) {
                        paren_depth -= 1;
                    } else if (c == '{') {
                        brace_depth += 1;
                    } else if (c == '}' and brace_depth > 0) {
                        brace_depth -= 1;
                    } else if (c == '=' and angle_depth == 0 and paren_depth == 0 and brace_depth == 0) {
                        break;
                    } else if (c == ';' or c == '\n') {
                        // End of statement without '='
                        break;
                    }
                    i += 1;
                }

                const type_end = i;
                const type_annotation = std.mem.trim(u8, content[type_start..type_end], " \t");

                // Skip whitespace before '='
                while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

                // Check for '='
                if (i < content.len and content[i] == '=') {
                    i += 1;
                    // Skip whitespace after '='
                    while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

                    // Check for $state (not $state. like $state.raw)
                    if (i + 6 < content.len and std.mem.eql(u8, content[i .. i + 6], "$state") and
                        (i + 6 >= content.len or content[i + 6] == '(' or content[i + 6] == '<'))
                    {
                        i += 6;

                        // Check if $state already has a generic parameter
                        const has_generic = i < content.len and content[i] == '<';

                        if (!has_generic and type_annotation.len > 0) {
                            // Transform: emit "let name = $state<Type>" instead of "let name: Type = $state"
                            // Emit "let " (keyword + space)
                            try result.appendSlice(allocator, content[decl_start..name_start]);
                            // Emit "name = $state<Type>"
                            try result.appendSlice(allocator, content[name_start..name_end]);
                            try result.appendSlice(allocator, " = $state<");
                            try result.appendSlice(allocator, type_annotation);
                            try result.appendSlice(allocator, ">");

                            // Continue from after "$state" (don't re-emit it)
                            continue;
                        }
                    }
                    // Not $state or already has generic, emit unchanged
                    try result.appendSlice(allocator, content[decl_start..i]);
                    continue;
                }
                // No '=', emit unchanged
                try result.appendSlice(allocator, content[decl_start..i]);
                continue;
            }
            // No type annotation, emit unchanged
            try result.appendSlice(allocator, content[decl_start..i]);
            continue;
        }

        // Regular character, copy it
        try result.append(allocator, content[i]);
        i += 1;
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
    // JavaScript files (no lang="ts") don't emit TypeScript-specific constructs
    try std.testing.expect(!virtual.is_typescript);
    try std.testing.expect(std.mem.endsWith(u8, virtual.virtual_path, ".js"));
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
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Component as __SvelteComponentType__, ComponentProps as __ComponentProps__ }") != null);
    // Snippet is globally available in Svelte 5 templates
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet }") != null);
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

    // Snippets are hoisted as Snippet-typed variables for assignability to Snippet props
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var greeting: Snippet<[any, any]>") != null);
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

    // Snippet is globally available in Svelte 5 templates
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "import type { Snippet }") != null);
    // Snippets are hoisted as Snippet-typed variables
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var shortcut: Snippet<[any, any]>") != null);
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

    // Snippets are hoisted as Snippet-typed variables (1 param)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var optionalParam: Snippet<[any]>") != null);
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

    // Snippets are typed as Snippet<[...]> for compatibility when passed to other snippets
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var child: Snippet<[any]>") != null);
}

test "snippet param optionality - default values and trailing commas" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Test cases from amp/server:
    // - Trailing comma shouldn't create extra param
    // - `name: Type = default` should be optional
    // - `name = default` should be optional
    // - Mix of required and optional params
    const source =
        \\<script lang="ts">
        \\    type Item = { id: string };
        \\</script>
        \\
        \\{#snippet trailingComma(
        \\    label: string,
        \\    isSelected: boolean,
        \\    onclick: () => void,
        \\)}
        \\    <button>{label}</button>
        \\{/snippet}
        \\
        \\{#snippet withDefault(props: Record<string, unknown> = {})}
        \\    <span>default</span>
        \\{/snippet}
        \\
        \\{#snippet mixedParams(title: string, count: number, expanded?: boolean, onToggle?: () => void)}
        \\    <div>{title}: {count}</div>
        \\{/snippet}
        \\
        \\{#snippet simpleDefault(user: Item, isTrigger = false)}
        \\    <span>{user.id}</span>
        \\{/snippet}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Snippets are now typed as Snippet<[any, ...]> for assignability to Snippet props.
    // Trailing comma: 3 params, not 4
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var trailingComma: Snippet<[any, any, any]>") != null);

    // Default value: 1 param
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var withDefault: Snippet<[any]>") != null);

    // Mixed: 4 params
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var mixedParams: Snippet<[any, any, any, any]>") != null);

    // Simple default: 2 params
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var simpleDefault: Snippet<[any, any]>") != null);
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

    // Should wrap instance script in generic render function
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function __render<T extends SomeInterface>()") != null);
}

test "generic component bindable props scoped inside __render" {
    // Regression test: $bindable() props must be visible in template expressions.
    // Previously, `void selected;` was emitted at module scope, but `selected`
    // is only declared inside __render for generic components.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts" generics="T">
        \\    interface Props {
        \\        items: T[];
        \\        selected?: T;
        \\    }
        \\    let { items, selected = $bindable() }: Props = $props();
        \\</script>
        \\
        \\{#each items as item}
        \\    <button onclick={() => selected = item}>
        \\        {item}
        \\    </button>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Generics.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The `void selected;` for bindable props must be INSIDE __render, not at module scope.
    // Find __render function start
    const render_start = std.mem.indexOf(u8, virtual.content, "function __render<T>()") orelse
        return error.TestUnexpectedResult;

    // Find the "// Component typing" comment which comes after __render closes
    const component_typing = std.mem.indexOf(u8, virtual.content, "// Component typing") orelse
        return error.TestUnexpectedResult;

    // Find `void selected;` for bindable props (look for the comment marker)
    const bindable_comment = std.mem.indexOf(u8, virtual.content, "// Mark bindable props") orelse
        return error.TestUnexpectedResult;

    // bindable void statement must be after __render starts and before "// Component typing"
    try std.testing.expect(bindable_comment > render_start);
    try std.testing.expect(bindable_comment < component_typing);

    // Also verify that onclick expression referencing selected is inside __render
    const onclick_expr = std.mem.indexOf(u8, virtual.content, "onclick: () => selected = item") orelse
        return error.TestUnexpectedResult;
    try std.testing.expect(onclick_expr > render_start);
    try std.testing.expect(onclick_expr < component_typing);
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

    // Should wrap instance script in generic render function
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "function __render<T>()") != null);
    // Should also have the module script content
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "export type ComboboxItem<T>") != null);
}

test "generic component imports hoisted to module level" {
    // Regression test: imports in generic components must be at module level,
    // not inside the __render function. Otherwise, tsgo reports false positive
    // "Cannot use import statement inside a function" errors.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts" generics="T">
        \\    import { tick } from 'svelte'
        \\    import type { Snippet } from 'svelte'
        \\    import * as Utils from '../utils'
        \\
        \\    let { items }: { items: T[] } = $props();
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Generic.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // Find positions of key markers
    const imports_hoisted = std.mem.indexOf(u8, virtual.content, "// <script> imports (hoisted)") orelse
        return error.TestUnexpectedResult;
    const render_func = std.mem.indexOf(u8, virtual.content, "function __render<T>()") orelse
        return error.TestUnexpectedResult;
    const tick_import = std.mem.indexOf(u8, virtual.content, "import { tick }") orelse
        return error.TestUnexpectedResult;
    const snippet_import = std.mem.indexOf(u8, virtual.content, "import type { Snippet }") orelse
        return error.TestUnexpectedResult;
    const utils_import = std.mem.indexOf(u8, virtual.content, "import * as Utils") orelse
        return error.TestUnexpectedResult;

    // All imports must be BEFORE the __render function (at module level)
    try std.testing.expect(imports_hoisted < render_func);
    try std.testing.expect(tick_import < render_func);
    try std.testing.expect(snippet_import < render_func);
    try std.testing.expect(utils_import < render_func);

    // The let { items } props declaration should be INSIDE __render
    const props_decl = std.mem.indexOf(u8, virtual.content, "let { items }") orelse
        return error.TestUnexpectedResult;
    try std.testing.expect(props_decl > render_func);
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

    // Should NOT emit hoisted declarations for names that shadow imports
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var filter: Snippet") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var map: Snippet") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var Default: Snippet") == null);

    // SHOULD emit hoisted Snippet declaration for snippet that doesn't shadow an import
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var other: Snippet<[]>") != null);

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
    // Snippet is hoisted as a Snippet-typed variable
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var child: Snippet<[any]>") != null);
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
    // For TypeScript files, we use __svelte_ensureArray which validates the iterable type
    // and handles null/undefined. For JS files, we use ?? [] fallback directly.
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

    // For TypeScript, we use __svelte_ensureArray for type validation
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "__svelte_ensureArray(items)[0]") != null);
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

test "regression: destructuring from empty object is preserved for type-checking" {
    // Regression test for ts-runes-hoistable-props-false-7.v5 conformance sample.
    // When destructuring from an empty object `{}`, TypeScript should report an error
    // like "Property 'destructured' does not exist on type '{}'.".
    // The generated TypeScript must preserve the original code so tsgo can detect this.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\    let { destructured } = {};
        \\    type Props = {
        \\        prop: typeof destructured;
        \\    };
        \\    let { prop }: Props = $props();
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "input.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The user's destructuring statement must be preserved for TypeScript to type-check
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let { destructured } = {};") != null);
}

test "JS Svelte files with undefined identifiers in $props defaults" {
    // Regression test for runes-best-effort-types.v5 conformance sample.
    // JavaScript Svelte files (no lang="ts") with undefined identifiers like
    // `let { g = foo } = $props()` should not cause type errors because
    // svelte-check doesn't strictly check JavaScript files for undefined names.
    //
    // The fix is in tsgo.zig's shouldSkipError() which skips "Cannot find name"
    // errors for JavaScript Svelte files (is_typescript_svelte = false).
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script>
        \\    let { a, b = true, g = foo } = $props(); 
        \\</script>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "input.svelte");
    const ast = try parser.parse();

    const virtual = try transform(allocator, ast);

    // The script content should be preserved as-is (including the undefined `foo`)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let { a, b = true, g = foo } = $props();") != null);
    // This is a JavaScript file, so is_typescript should be false
    try std.testing.expect(!virtual.is_typescript);
}

/// Detect the first TypeScript syntax in a JavaScript script.
/// Returns the offset (within the script content) where TypeScript syntax was found.
/// Used to emit "Unexpected token" diagnostic for JS scripts with TS syntax.
///
/// Detects:
/// - Type annotations: `let x: Type` - returns position of `:`
/// - Type assertions: `value as Type` - returns position of `as`
fn findFirstTsSyntax(content: []const u8) ?usize {
    var i: usize = 0;

    while (i < content.len) {
        const c = content[i];

        // Skip strings
        if (c == '"' or c == '\'' or c == '`') {
            i = skipString(content, i);
            continue;
        }

        // Skip comments
        if (i + 1 < content.len and c == '/' and content[i + 1] == '/') {
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            continue;
        }
        if (i + 1 < content.len and c == '/' and content[i + 1] == '*') {
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

        // Check for type annotation: colon after identifier (but not in ternary or object literal)
        // Pattern: identifier : Type (in let/const/param context)
        if (c == ':') {
            // Look back to see if this is after an identifier (type annotation context)
            if (isTypeAnnotationColon(content, i)) {
                return i;
            }
        }

        // Check for type assertion: `as` keyword followed by type
        // Must be after an expression, not part of import
        if (startsWithKeyword(content[i..], "as") and i > 0) {
            // Make sure it's not `import { x as y }` or `export { x as y }` pattern
            if (!isImportAsContext(content, i)) {
                return i;
            }
        }

        i += 1;
    }

    return null;
}

/// Check if a colon at position `pos` is a type annotation rather than object literal key or ternary
fn isTypeAnnotationColon(content: []const u8, pos: usize) bool {
    if (pos == 0) return false;

    // Scan back past optional whitespace
    var j = pos - 1;
    while (j > 0 and (content[j] == ' ' or content[j] == '\t')) : (j -= 1) {}

    // Must be after an identifier
    if (!isIdentChar(content[j])) return false;

    // Scan back to find start of identifier
    const ident_end = j + 1;
    while (j > 0 and isIdentChar(content[j - 1])) : (j -= 1) {}
    const ident_start = j;

    // Now look for preceding context
    // Scan back past whitespace (but NOT newlines - those we want to detect)
    var k = ident_start;
    while (k > 0 and (content[k - 1] == ' ' or content[k - 1] == '\t')) : (k -= 1) {}

    // Check what's before the identifier
    if (k == 0) {
        // At start of content, check if line starts with let/const/var
        return isVarDeclarationLine(content[0..ident_end]);
    }

    const prev_char = content[k - 1];

    // Definitely not type annotation: object literal key `{key:` or ternary `? :`
    if (prev_char == '?') return false;

    // After `=` could be a cast: `let x = value: Type` - but that's not valid TS either
    // More likely we're in object literal `{key: value}` if prev is `{` or `,`
    if (prev_char == '{') return false;

    // After `,` could be object literal or variable list - check for var keyword
    if (prev_char == ',') {
        return hasVarKeywordBefore(content, k);
    }

    // After `(` could be function param
    if (prev_char == '(') return true;

    // After newline - check if current line is a variable declaration
    if (prev_char == '\n') {
        // Find the line and check if it starts with let/const/var
        const line_content = content[k..ident_end];
        return isVarDeclarationLine(line_content);
    }

    // Check if directly preceded by let/const/var keyword
    // At this point, k points to the space AFTER let/const/var (we scanned back past whitespace)
    // So content[k] is that space, and content[k-3..k] should be "let" or "var"
    // Or content[k-5..k] should be "const"
    if (k >= 3 and k < content.len and content[k] == ' ') {
        if (std.mem.eql(u8, content[k - 3 .. k], "let")) return true;
        if (std.mem.eql(u8, content[k - 3 .. k], "var")) return true;
    }
    if (k >= 5 and k < content.len and content[k] == ' ') {
        if (std.mem.eql(u8, content[k - 5 .. k], "const")) return true;
    }

    // Otherwise, assume it's an object literal key
    return false;
}

/// Check if content starts with let/const/var (possibly with leading whitespace)
fn isVarDeclarationLine(content: []const u8) bool {
    var i: usize = 0;
    while (i < content.len and (content[i] == ' ' or content[i] == '\t')) : (i += 1) {}

    if (startsWithKeyword(content[i..], "let")) return true;
    if (startsWithKeyword(content[i..], "const")) return true;
    if (startsWithKeyword(content[i..], "var")) return true;
    return false;
}

/// Look backwards from position to find if there's a let/const/var on the same statement
fn hasVarKeywordBefore(content: []const u8, pos: usize) bool {
    // Simple heuristic: look for let/const/var with no semicolon between
    var search_start: usize = 0;
    if (pos >= 100) search_start = pos - 100;

    const context = content[search_start..pos];

    // Find last semicolon or newline (statement boundary)
    var boundary: usize = 0;
    for (context, 0..) |ch, idx| {
        if (ch == ';' or ch == '\n') boundary = idx + 1;
    }

    // Now check if the segment after boundary starts with let/const/var
    const stmt = std.mem.trimLeft(u8, context[boundary..], " \t");
    return std.mem.startsWith(u8, stmt, "let ") or
        std.mem.startsWith(u8, stmt, "const ") or
        std.mem.startsWith(u8, stmt, "var ");
}

/// Check if `as` at position is part of import/export renaming (not type assertion)
fn isImportAsContext(content: []const u8, pos: usize) bool {
    // Look for import or export before this position
    var search_start: usize = 0;
    if (pos >= 200) search_start = pos - 200;

    const context = content[search_start..pos];

    // Find the last statement boundary (newline or semicolon)
    var i: usize = context.len;
    while (i > 0) {
        i -= 1;
        if (context[i] == ';') break;
        if (context[i] == '\n') break;
    }

    const stmt = std.mem.trim(u8, context[i..], " \t\n\r");

    // If statement starts with import or export, this is renaming context
    return std.mem.startsWith(u8, stmt, "import") or std.mem.startsWith(u8, stmt, "export");
}

test "multi-line attribute with mixed expressions" {
    // Regression test: multi-line class attribute with multiple expressions
    // should be emitted as a template literal, not a broken string
    const source =
        \\<script lang="ts">
        \\  let hasBorder = true;
        \\  let isCustomer = false;
        \\</script>
        \\
        \\<div
        \\  class="p-4 {hasBorder ? 'border' : ''} {isCustomer
        \\    ? 'bg-gray-50'
        \\    : ''}"
        \\>
        \\  Test
        \\</div>
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var parser = @import("svelte_parser.zig").Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const result = try transform(allocator, ast);

    // Should not contain broken string literals (double quotes spanning multiple lines)
    // Should use template literals (backticks) for mixed values
    const has_template_literal = std.mem.indexOf(u8, result.content, "`p-4 ${") != null or
        std.mem.indexOf(u8, result.content, "`") != null;
    try std.testing.expect(has_template_literal);
}

test "multi-line static attribute" {
    // Regression test: multi-line style attribute without expressions
    const source =
        \\<script lang="ts">
        \\</script>
        \\
        \\<div
        \\  style="
        \\    background-image: radial-gradient(
        \\      ellipse 60% 50%,
        \\      transparent 100%
        \\    );
        \\  "
        \\>
        \\  Test
        \\</div>
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var parser = @import("svelte_parser.zig").Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const result = try transform(allocator, ast);

    // Should use template literal for multi-line value
    const has_template_literal = std.mem.indexOf(u8, result.content, "style: `") != null;
    try std.testing.expect(has_template_literal);
}

test "svelte 5 attach directive parsing" {
    // Regression test: {@attach ...} should be parsed correctly and skipped
    const source =
        \\<script lang="ts">
        \\  function tooltip(msg: string) { return {}; }
        \\  let value = 10;
        \\</script>
        \\
        \\<span
        \\  class="foo"
        \\  {@attach tooltip(`Value: ${value}`)}
        \\>
        \\  Hover me
        \\</span>
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var parser = @import("svelte_parser.zig").Parser.init(allocator, source, "test.svelte");
    const ast = try parser.parse();

    const result = try transform(allocator, ast);

    // Should not contain 'attach' as a standalone identifier reference
    // (the expression inside @attach should not leak identifiers)
    const has_attach_ref = std.mem.indexOf(u8, result.content, "void attach;") != null;
    try std.testing.expect(!has_attach_ref);
}

// TODO: Spread attribute parsing in the parser is not yet implemented.
// The parser currently skips over {...} in attribute positions.
// Spread identifiers are extracted via scanTemplateForComponents instead.
// Uncomment and implement when proper spread attribute parsing is added.
//
// test "spread attribute parsing" {
//     // Regression test: {...expr} should be parsed as spread and skipped
//     const source =
//         \\<script lang="ts">
//         \\  let props = { class: 'foo' };
//         \\</script>
//         \\
//         \\<div {...props}>
//         \\  Test
//         \\</div>
//     ;
//
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();
//     var parser = @import("svelte_parser.zig").Parser.init(allocator, source, "test.svelte");
//     const ast = try parser.parse();
//
//     // Check that the parser found the spread attribute
//     try std.testing.expect(ast.elements.items.len >= 1);
//     const elem = ast.elements.items[0];
//     const attrs = ast.attributes.items[elem.attrs_start..elem.attrs_end];
//
//     var found_spread = false;
//     for (attrs) |attr| {
//         if (std.mem.eql(u8, attr.name, "...")) {
//             found_spread = true;
//             break;
//         }
//     }
//     try std.testing.expect(found_spread);
// }

test "extractDestructuredNames extracts from object pattern" {
    var params: std.StringHashMapUnmanaged(void) = .empty;
    defer params.deinit(std.testing.allocator);

    try extractDestructuredNames(std.testing.allocator, " submit, form: enhanceFormEl ", &params);

    try std.testing.expect(params.contains("submit"));
    try std.testing.expect(params.contains("enhanceFormEl"));
    try std.testing.expect(!params.contains("form"));
}

test "collectArrowFunctionParamsAndRegions handles async destructuring" {
    var params: std.StringHashMapUnmanaged(void) = .empty;
    defer params.deinit(std.testing.allocator);
    var regions: std.ArrayList(ParamRegion) = .empty;
    defer regions.deinit(std.testing.allocator);

    const expr = "async ({ submit, form: enhanceFormEl }) => { await submit(); }";
    try collectArrowFunctionParamsAndRegions(std.testing.allocator, expr, &params, &regions);

    try std.testing.expect(params.contains("submit"));
    try std.testing.expect(params.contains("enhanceFormEl"));
    // Should have one region for the parameter list
    try std.testing.expect(regions.items.len >= 1);
}

test "extractIdentifiersFromExpr excludes async arrow destructured params" {
    const allocator = std.testing.allocator;
    var refs: std.StringHashMapUnmanaged(u32) = .empty;
    defer refs.deinit(allocator);

    const expr = "{...formApi.enhance(async ({ submit, form: enhanceFormEl }) => { await submit(); enhanceFormEl.reset(); })}";
    try extractIdentifiersFromExpr(allocator, expr, 0, &refs);

    // formApi should be extracted (it's the actual identifier reference)
    try std.testing.expect(refs.contains("formApi"));
    // submit and enhanceFormEl should NOT be extracted (they're arrow params)
    try std.testing.expect(!refs.contains("submit"));
    try std.testing.expect(!refs.contains("enhanceFormEl"));
    // form should NOT be extracted (it's a property key, not a binding)
    try std.testing.expect(!refs.contains("form"));
}

test "transform {:then} bindings with type inference from await expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  async function fetchData(): Promise<{ count: number }> {
        \\    return { count: 42 };
        \\  }
        \\  let promise = fetchData();
        \\</script>
        \\{#await promise}
        \\  <p>Loading...</p>
        \\{:then result}
        \\  <p>{result.count}</p>
        \\{/await}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // Should emit `let result = await (promise);` to infer type from promise
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let result = await (promise);") != null);
    // Should NOT emit `let result: unknown;`
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "let result: unknown") == null);
}

test "transform {:then} destructuring with type inference from await expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\  let dataPromise: Promise<{ items: string[]; total: number }> | null = null;
        \\</script>
        \\{#if dataPromise}
        \\  {#await dataPromise}
        \\    <p>Loading...</p>
        \\  {:then { items, total }}
        \\    <p>{total} items</p>
        \\  {/await}
        \\{/if}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // Should emit `var { items, total } = await (dataPromise);` to infer type
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "var { items, total } = await (dataPromise);") != null);
    // Should NOT emit `var { items, total } = {} as any;`
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "= {} as any") == null);
}

test "snippet body expressions with apostrophes in text content" {
    // Regression test: apostrophes in text content (like "Owner's Manual") were being
    // treated as string delimiters, causing findSnippetBodyRanges to skip over {/snippet}.
    // This resulted in snippet body expressions being emitted at module scope instead of
    // inside the wrapper function, causing "Cannot find name" errors for snippet parameters.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let Popover = {} as any;
        \\</script>
        \\<Popover.Content>
        \\    {#snippet child({ open: isOpen })}
        \\        {#if isOpen}
        \\            <span>Owner's Manual</span>
        \\        {/if}
        \\    {/snippet}
        \\</Popover.Content>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // Should NOT have void (isOpen); at module scope
    // It should be inside the wrapper function
    const bad_pattern = ";// Template expressions\nvoid (isOpen);";
    const has_bad = std.mem.indexOf(u8, virtual.content, bad_pattern) != null;
    try std.testing.expect(!has_bad);

    // Should have the wrapper function with isOpen in scope
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "(({ open: isOpen })") != null);
}

test "@const declarations emitted before usages in each blocks" {
    // Regression test: {@const} declarations inside {#each} blocks should be
    // emitted BEFORE template expressions that use them. Otherwise TypeScript
    // reports "Cannot find name" errors because the variable is used before declaration.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let items = [{ name: 'a' }];
        \\</script>
        \\
        \\{#each items as item}
        \\  {@const name = item.name}
        \\  <div>{name}</div>
        \\{/each}
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // The const declaration MUST appear before its usage
    const const_decl = "var name = item.name;";
    const void_usage = "void (name);";

    const decl_pos = std.mem.indexOf(u8, virtual.content, const_decl);
    const usage_pos = std.mem.indexOf(u8, virtual.content, void_usage);

    try std.testing.expect(decl_pos != null);
    try std.testing.expect(usage_pos != null);
    // Declaration must come before usage
    try std.testing.expect(decl_pos.? < usage_pos.?);
}

test "IIFE local variables should not be extracted to module scope" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let position = { left: 100 };
        \\</script>
        \\<div style={`left: ${(() => {
        \\  const menuWidth = 425;
        \\  const leftPos = Math.min(position.left, 100);
        \\  return leftPos;
        \\})()}px`}></div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();
    const virtual = try transform(allocator, ast);

    // IIFE-local variables should NOT appear as void statements
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void menuWidth") == null);
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void leftPos") == null);
    // The IIFE itself should still be in the template expression (for type-checking)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "const menuWidth = 425") != null);
}

test "function expression local variables should not be extracted to module scope" {
    const allocator = std.testing.allocator;
    var refs: std.StringHashMapUnmanaged(u32) = .empty;
    defer refs.deinit(allocator);

    const expr = "(() => { const localVar = 1; return localVar; })()";
    try extractIdentifiersFromExpr(allocator, expr, 0, &refs);

    // localVar should NOT be extracted (it's inside the function body)
    try std.testing.expect(!refs.contains("localVar"));
}

test "regular function local variables should not be extracted to module scope" {
    const allocator = std.testing.allocator;
    var refs: std.StringHashMapUnmanaged(u32) = .empty;
    defer refs.deinit(allocator);

    const expr = "(function() { const inner = 1; return inner; })()";
    try extractIdentifiersFromExpr(allocator, expr, 0, &refs);

    // inner should NOT be extracted (it's inside the function body)
    try std.testing.expect(!refs.contains("inner"));
}

test "class directive with hyphen in name should not extract class name as identifier" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let isCollapsed = true;
        \\</script>
        \\<div class:pb-1.5={isCollapsed}>test</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    // Verify parser correctly captured the full class name with value
    try std.testing.expectEqual(@as(usize, 1), ast.attributes.items.len);
    try std.testing.expectEqualStrings("class:pb-1.5", ast.attributes.items[0].name);
    try std.testing.expectEqualStrings("{isCollapsed}", ast.attributes.items[0].value.?);

    const virtual = try transform(allocator, ast);

    // The class directive condition (isCollapsed) should be extracted
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void isCollapsed") != null);
    // The class name (pb-1.5 or pb-1 or pb) should NOT be extracted as an identifier
    // This would cause "void pb-1;" which TypeScript parses as "void pb - 1;" (syntax error)
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void pb") == null);
}

test "shorthand class directive extracts variable name" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let hidden = true;
        \\</script>
        \\<div class:hidden>test</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    // Verify parser captured shorthand class directive
    try std.testing.expectEqual(@as(usize, 1), ast.attributes.items.len);
    try std.testing.expectEqualStrings("class:hidden", ast.attributes.items[0].name);
    try std.testing.expect(ast.attributes.items[0].value == null);

    const virtual = try transform(allocator, ast);

    // Shorthand class:hidden should extract "hidden" as a variable reference
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void hidden") != null);
}

test "class directive with Tailwind arbitrary value brackets" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source =
        \\<script lang="ts">
        \\let active = true;
        \\</script>
        \\<div class:bg-[#fff]={active}>test</div>
    ;

    const Parser = @import("svelte_parser.zig").Parser;
    var parser = Parser.init(allocator, source, "Test.svelte");
    const ast = try parser.parse();

    // Verify parser captured Tailwind arbitrary value class name
    try std.testing.expectEqual(@as(usize, 1), ast.attributes.items.len);
    try std.testing.expectEqualStrings("class:bg-[#fff]", ast.attributes.items[0].name);
    try std.testing.expectEqualStrings("{active}", ast.attributes.items[0].value.?);

    const virtual = try transform(allocator, ast);

    // The condition variable should be extracted
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void active") != null);
    // The class name should NOT be extracted as identifiers
    try std.testing.expect(std.mem.indexOf(u8, virtual.content, "void bg") == null);
}
