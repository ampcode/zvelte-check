<!--
  Test {#each} with any-typed values.
  This is a regression test for the "is of type 'unknown'" false positives.
  When iterating over any-typed arrays (e.g., from $props() without type annotation),
  items should be 'any' not 'unknown', allowing property access without errors.
-->
<script lang="ts">
    // Simulate SvelteKit page data without explicit typing
    // This creates an 'any' typed data object
    let { data } = $props<{ data: any }>();
    
    // Typed array for comparison  
    type Item = { slug: string; title: string };
    let typedItems: Item[] = [];
</script>

<!-- any-typed array - should not error -->
{#each data.items as item (item.slug)}
    <p>{item.title}</p>
{/each}

<!-- Typed array - should work and preserve type -->
{#each typedItems as item (item.slug)}
    <p>{item.title}</p>
{/each}

<!-- Nested access on any -->
{#each data.nested.deeply.items as item}
    <span>{item.value}</span>
{/each}
