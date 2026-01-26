<script lang="ts">
  import Card from './Card.svelte';
  let items = [1, 2, 3];
  let promise = Promise.resolve(42);
</script>

<!-- Snippets inside {#if} should NOT be treated as Card props -->
<Card class="test-if">
  {#if true}
    {#snippet ifSnippet(label: string)}
      <button>{label}</button>
    {/snippet}
    <div>
      {@render ifSnippet("Click me")}
    </div>
  {/if}
</Card>

<!-- Snippets inside {#each} should NOT be treated as Card props -->
<Card class="test-each">
  {#each items as item}
    {#snippet eachSnippet(value: number)}
      <span>{value}</span>
    {/snippet}
    {@render eachSnippet(item)}
  {/each}
</Card>

<!-- Snippets inside {#await} should NOT be treated as Card props -->
<Card class="test-await">
  {#await promise}
    {#snippet awaitSnippet()}
      <span>Loading...</span>
    {/snippet}
    {@render awaitSnippet()}
  {/await}
</Card>

<!-- Direct child snippets SHOULD be treated as Card props -->
<Card class="test-direct">
  {#snippet directSnippet()}
    <span>Direct</span>
  {/snippet}
  {@render directSnippet()}
</Card>
