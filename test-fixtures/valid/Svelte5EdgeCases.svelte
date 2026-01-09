<!--
  Svelte 5 syntax edge cases.
  Tests parser handling of new Svelte 5 features.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';
    
    interface Props {
        items: string[];
        header?: Snippet<[{ title: string }]>;
        row?: Snippet<[item: string, index: number]>;
    }
    
    let { items, header, row }: Props = $props();
    
    let value = $state("");
    let count = $state(0);
    
    // Derived with getter function
    let doubled = $derived.by(() => {
        return count * 2;
    });
</script>

<!-- Snippets with typed parameters -->
{#snippet typedSnippet(name: string, age: number)}
    <p>{name} is {age} years old</p>
{/snippet}

<!-- Snippet with optional parameter -->
{#snippet optionalParam(name?: string)}
    <p>Hello {name ?? 'World'}</p>
{/snippet}

<!-- Snippet with default value -->
{#snippet defaultParam(greeting = 'Hello')}
    <p>{greeting}!</p>
{/snippet}

<!-- Snippet with complex type -->
{#snippet complexType(item: { id: number; name: string })}
    <p>{item.id}: {item.name}</p>
{/snippet}

<!-- Nested snippet in control flow -->
{#if items.length > 0}
    {#snippet localSnippet()}
        <span>Local to if block</span>
    {/snippet}
    {@render localSnippet()}
{/if}

{#each items as item, index}
    {#snippet eachSnippet()}
        <span>{item} at {index}</span>
    {/snippet}
    {@render eachSnippet()}
{/each}

<!-- Render with expressions -->
{@render typedSnippet("Alice", 30)}
{@render optionalParam()}
{@render defaultParam()}

<!-- Conditional rendering -->
{#if header}
    {@render header({ title: "My Title" })}
{/if}

{#if row}
    {#each items as item, i}
        {@render row(item, i)}
    {/each}
{/if}
