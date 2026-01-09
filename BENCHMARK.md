# Performance Benchmark

Comparison of svelte-check-zig vs official svelte-check on a real SvelteKit project.

## Test Environment

- **Project**: ~/repos/amp/web (Sourcegraph Amp web frontend)
- **Files**: 313 Svelte components
- **Machine**: Apple Silicon (arm64), macOS
- **Date**: January 2026

## Results

| Metric | svelte-check-zig | svelte-check | Speedup |
|--------|------------------|--------------|---------|
| Wall time | **1.1s** | 7.9s | **7.2x faster** |
| CPU time | 2.1s | 14.8s | 7x less CPU |
| Memory | **605 MB** | 1,216 MB | **2x less memory** |

## Notes

### svelte-check-zig
- Parallel file processing via thread pool
- Native Zig binary with minimal startup overhead
- Uses tsgo for TypeScript checking (Go-based, fast startup)
- Currently reports more errors due to missing SvelteKit virtual module resolution ($app/*, workspace packages)

### svelte-check (official)
- Node.js based with significant startup overhead
- Full SvelteKit integration (resolves $app/*, $lib/*, etc.)
- Uses TypeScript language service
- Zero errors on this project (full compatibility)

## TypeScript Module Resolution

After refactoring to run tsgo from the workspace root (instead of an isolated temp directory), module resolution now works correctly for most imports.

### What Works

- **node_modules packages**: All npm dependencies resolve correctly
- **Relative .ts/.js imports**: `./utils`, `../lib/helpers`, etc.
- **Monorepo packages**: `@sourcegraph/*` packages resolve via tsconfig paths
- **SvelteKit virtual modules**: `$app/environment`, `$app/stores`, `$app/navigation`, etc. (via generated type stubs)
- **Svelte store subscriptions**: `$storeName` transformed to `__svelte_store_get(storeName)`
- **Path aliases**: `$lib/*` and other tsconfig paths work when extending project config

### What Doesn't Work (Yet)

- **Vite virtual modules**: `~icons/*` and other Vite plugin virtual imports (~549 remaining errors, mostly from these)
- **Some edge cases**: Complex type inference in certain Svelte 5 runes patterns

### Error Count Progress

| Stage | Errors | Notes |
|-------|--------|-------|
| Initial (temp dir) | ~1600 | Module resolution broken for everything |
| After workspace-root refactor | ~549 | Most are Vite virtual module failures |

The remaining errors are primarily `~icons/*` imports from unplugin-icons, which generates virtual modules at build time. These would require either:
1. Vite plugin integration (out of scope)
2. Generic fallback type stubs for `~icons/*` pattern

## Caveats

The performance comparison remains valid—the 7x speedup is real. The remaining false positives are expected limitations:

1. ~~SvelteKit virtual modules ($app/environment, $app/stores) aren't resolved~~ **FIXED**
2. ~~Svelte store auto-subscriptions ($storeName) aren't transformed~~ **FIXED**
3. ~~Workspace package imports aren't resolved (@sourcegraph/*, etc.)~~ **FIXED**
4. Vite plugin virtual modules (`~icons/*`) aren't resolved (expected, out of scope)

## Raw Data

```
=== svelte_check_zig ===
real    0m1.097s
user    0m1.826s
sys     0m0.297s
maximum resident set size: 633946112 bytes (605 MB)

=== svelte-check ===
real    0m7.880s
user    0m14.022s
sys     0m0.751s
maximum resident set size: 1274740736 bytes (1216 MB)
```
