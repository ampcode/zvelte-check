<!--
  Generic component type errors.
  Tests TypeScript generics with Svelte components.
-->
<script lang="ts" generics="T, U extends string">
    import type { Snippet } from 'svelte';

    interface Props<T, U extends string> {
        items: T[];
        keys: U[];
        transform: (item: T) => U;
        children?: Snippet<[{ item: T; key: U }]>;
    }

    let { items, keys, transform, children }: Props<T, U> = $props();

    // Generic constraint violation
    // Expected: line 18, code: TS2344, message: Type 'number' does not satisfy the constraint 'string'
    type InvalidKey = Props<string, number>;

    // Wrong type for generic parameter
    // Expected: line 23, code: TS2345, message: Argument of type 'string' is not assignable
    function processItem(item: T): void {
        const result: number = transform(item);
    }

    // Generic array type mismatch
    // Expected: line 28, code: TS2322, message: Type 'U' is not assignable to type 'T'
    const mixed: T[] = [...items, ...keys];

    // Calling generic function with wrong types
    function identity<V>(value: V): V {
        return value;
    }
    // Expected: line 35, code: TS2345, message: Argument of type 'T' is not assignable
    const stringResult: string = identity<string>(items[0]);
</script>

<!-- Using component with incompatible generic types -->
<!-- Expected: (multiple errors from generic constraint violations) -->
{#each items as item, i}
    <div>
        <!-- Expected: line 44, code: TS2345, message: Argument of type -->
        {transform(keys[i])}
    </div>
{/each}

{#if children}
    <!-- Expected: line 50, code: TS2322, message: Type mismatch in snippet props -->
    {@render children({ item: "wrong", key: 123 })}
{/if}
