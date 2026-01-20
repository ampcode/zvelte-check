# Debugging False Positives vs svelte-check

This document describes the methodology for finding and fixing diagnostic discrepancies between zvelte-check and svelte-check.

## Quick Reference

```bash
# Compare outputs on a specific file
cd test-fixtures/comparison
pnpm exec svelte-check --output machine 2>&1 | grep -i filename
../../zig-out/bin/zvelte-check --output machine 2>&1 | grep -i filename

# Dump generated TypeScript from BOTH tools (the key debugging step)
zig build dump -- path/to/File.svelte          # zvelte-check output
./scripts/dump-svelte2tsx.sh path/to/File.svelte  # svelte2tsx output

# Side-by-side comparison
diff <(zig build dump -- File.svelte) <(./scripts/dump-svelte2tsx.sh File.svelte)

# Run conformance tests
./scripts/conformance-test.sh
```

## Step-by-Step Debugging Process

### 1. Identify the discrepancy

Run both tools and compare:

```bash
cd test-fixtures/comparison  # or create a minimal reproduction
pnpm exec svelte-check --output machine
../../zig-out/bin/zvelte-check --output machine
```

Note which errors appear in one but not the other:
- **False positive**: zvelte-check reports, svelte-check doesn't
- **False negative**: svelte-check reports, zvelte-check doesn't

### 2. Create a minimal reproduction

Create a small `.svelte` file in `test-fixtures/comparison/` that isolates the issue.

### 3. Compare generated TypeScript

This is the key step. Both tools work by transforming Svelte to TypeScript:

```
.svelte → transformer → virtual .ts → TypeScript checker → diagnostics
```

**Get output from both tools:**

```bash
# zvelte-check transformer output
zig build dump -- path/to/YourFile.svelte

# svelte2tsx output (what svelte-check uses)
./scripts/dump-svelte2tsx.sh path/to/YourFile.svelte

# Side-by-side diff
diff <(zig build dump -- YourFile.svelte 2>/dev/null) \
     <(./scripts/dump-svelte2tsx.sh YourFile.svelte 2>/dev/null)
```

### 4. Analyze the difference

Common issues to look for:

| Symptom | Likely Cause |
|---------|--------------|
| "Cannot find name 'X'" | Variable not in scope where expression is emitted |
| "Property 'X' does not exist" | Wrong type for element/component |
| Missing error | Expression not being type-checked at all |
| Duplicate error | Expression emitted multiple times |

### 5. Understand svelte2tsx's approach

Key patterns svelte2tsx uses:

- **Render function wrapper**: `function $$render() { ... }`
- **Async wrapper for template**: `async () => { ... }` inside render
- **Control flow preservation**: `for(let item of ...)` stays as loops
- **Element creation**: `svelteHTML.createElement("button", { onclick: ... })`
- **Hoistable snippets**: Snippets without instance references go to module scope

### 6. Fix the transformer

Edit `src/transformer.zig` to match svelte2tsx's behavior for the specific case.

### 7. Add a regression test

Add the minimal reproduction to `test-fixtures/` and ensure the conformance test passes:

```bash
./scripts/conformance-test.sh
```

## Useful Resources

- **svelte2tsx source**: https://github.com/sveltejs/language-tools/tree/master/packages/svelte2tsx
- **svelte2tsx test samples**: `packages/svelte2tsx/test/svelte2tsx/samples/`
- **Our conformance tests**: `test-fixtures/conformance/samples/`
- **Comparison script**: `scripts/compare-tools.sh`

## Example: The $props() Scoping Bug

**Issue**: `Cannot find name 'selected'` in `onclick={() => selected = item}`

**Discovery process**:
1. Ran `zig build test` - saw "FALSE POSITIVE: Generics.svelte:10:24"
2. Compared svelte-check output (0 errors) vs zvelte-check (1 error)
3. Dumped svelte2tsx output - saw props inside `$$render()` with template in nested `async () => {}`
4. Dumped our output - saw props inside function but template expressions as flat `void` statements
5. Root cause: our template expressions aren't nested in the same scope as the props

**Fix approach**: Either emit template expressions inline within control flow, or ensure props are visible at template expression scope.
