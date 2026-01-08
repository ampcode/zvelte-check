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

## Caveats

The svelte-check-zig tool currently reports false positives because:
1. ~~SvelteKit virtual modules ($app/environment, $app/stores) aren't resolved~~ **FIXED**: Type stubs for `$app/*` modules are now generated
2. ~~Svelte store auto-subscriptions ($storeName) aren't transformed~~ **FIXED**: `$storeName` is now transformed to `__svelte_store_get(storeName)`
3. Workspace package imports aren't resolved (@sourcegraph/*, etc.)

These are expected limitations documented in the project scope. The performance comparison remains valid for the parsing, a11y checking, and TypeScript invocation portions.

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
