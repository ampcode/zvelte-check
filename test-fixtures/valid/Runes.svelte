<!--
  Valid component demonstrating Svelte 5 runes.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Props {
        initialCount?: number;
        multiplier?: number;
        children?: Snippet;
        header?: Snippet<[{ count: number }]>;
    }

    let { initialCount = 0, multiplier = 1, children, header }: Props = $props();

    // $state rune for reactive state
    let count = $state(initialCount);
    let history: number[] = $state([]);

    // $derived rune for computed values
    let doubled = $derived(count * 2);
    let multiplied = $derived(count * multiplier);
    let total = $derived.by(() => {
        return history.reduce((sum, n) => sum + n, 0);
    });

    // $effect rune for side effects
    $effect(() => {
        console.log(`Count changed to ${count}`);
    });

    $effect.pre(() => {
        // Runs before DOM updates
    });

    function increment() {
        count += 1;
        history.push(count);
    }

    function reset() {
        count = initialCount;
        history = [];
    }
</script>

<div class="runes-demo">
    {#if header}
        {@render header({ count })}
    {:else}
        <h2>Runes Demo</h2>
    {/if}

    <p>Count: {count}</p>
    <p>Doubled: {doubled}</p>
    <p>Multiplied (×{multiplier}): {multiplied}</p>
    <p>History total: {total}</p>

    <button type="button" onclick={increment}>Increment</button>
    <button type="button" onclick={reset}>Reset</button>

    {#if children}
        <div class="children">
            {@render children()}
        </div>
    {/if}
</div>

<style>
    .runes-demo {
        padding: 1rem;
        border: 1px solid #ccc;
        border-radius: 4px;
    }

    .children {
        margin-top: 1rem;
        padding-top: 1rem;
        border-top: 1px solid #eee;
    }

    button {
        margin-right: 0.5rem;
    }
</style>
