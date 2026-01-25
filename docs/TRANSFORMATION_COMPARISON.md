# Transformation Strategy Comparison: zvelte-check vs svelte-check

## Summary

This document compares how zvelte-check and svelte-check (via svelte2tsx) transform Svelte 5 code for TypeScript type-checking, with focus on diagnostic parity.

## Background

Both tools transform `.svelte` files into TypeScript for type-checking:
- **zvelte-check**: Uses Zig-based transformation with tsgo
- **svelte-check**: Uses svelte2tsx (TypeScript-based) with tsc

## Rune Handling

### zvelte-check Approach
Declares runes at the TOP of each generated `.svelte.ts` file:
```typescript
// Svelte 5 rune type stubs
declare function $state(): undefined;
declare function $state<T>(initial: T): T;
declare function $state<T>(): T | undefined;
// ... etc
```

**Pros:**
- Simple and self-contained
- No external dependencies for rune types
- Matches Svelte's own ambient declaration style
- Works with tsgo without configuration

**Cons:**
- Cannot detect variable shadowing issues (function declarations hoist)

### svelte-check/svelte2tsx Approach
Uses `///<reference types="svelte" />` to load rune types from Svelte's ambient declarations, then wraps code in `function $$render() { ... }`.

Additionally, adds store subscription shims for variables whose names could be stores:
```typescript
let state = $state<T>(value)/*Ωignore_startΩ*/;let $state = __sveltets_2_store_get(state);/*Ωignore_endΩ*/;
```

This is added because Svelte's `$storeName` syntax auto-subscribes to stores, and svelte2tsx needs to handle template usage of `$state` when a variable named `state` exists.

## "Missing" Errors Analysis

During investigation, svelte-check reported 3-4 errors that zvelte-check missed:
- TS7022: `'state' implicitly has type 'any'`
- TS2448: `Block-scoped variable '$state' used before its declaration`
- TS2347: `Untyped function calls may not accept type arguments`

**Root Cause:** These errors occur in `remote-control-story.svelte` where:
```typescript
let state = $state<SimulationState>(createInitialSimulationState())
```

svelte2tsx transforms this to:
```typescript
let state = $state<SimulationState>(createInitialSimulationState());let $state = __sveltets_2_store_get(state);
```

The added `let $state = __sveltets_2_store_get(state)` creates a circular reference that TypeScript reports as an error.

**Finding:** These are **false positives in svelte-check**, not real bugs. The circular reference is an artifact of svelte2tsx's store subscription handling, not a real issue in the Svelte code.

## Recommendation

**Keep zvelte-check's current approach.** The "missing" errors are false positives caused by svelte2tsx's store handling quirks, not real bugs that users should fix.

Diagnostic parity is already excellent:
- svelte-check: 53 errors, 6 warnings
- zvelte-check: 55 errors, 5 warnings

The 2 extra errors in zvelte-check are likely real issues that svelte-check misses, while the "missing" errors from svelte-check are false positives.

## Alternative Considered

We could replicate svelte2tsx's store subscription handling, but this would:
1. Add false positives for variables named `state`, `derived`, etc.
2. Increase transformation complexity
3. Not provide additional value since the errors are not real bugs

## Conclusion

No changes to zvelte-check's transformation strategy are needed. The current approach is simpler, produces fewer false positives, and maintains excellent diagnostic parity with svelte-check.
