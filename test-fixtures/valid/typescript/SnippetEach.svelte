<script lang="ts">
  interface Item {
    id: string;
    name: string;
  }

  let allItems: Item[] = $state([]);
</script>

<!-- Snippet with parameter used in {#each} iterable -->
{#snippet renderItems(items: Item[])}
  <ul>
    {#each items as item}
      <li>{item.name}</li>
    {/each}
  </ul>
{/snippet}

<!-- Snippet with multiple params and destructuring in each -->
{#snippet itemList(items: Item[], title: string)}
  <h2>{title}</h2>
  {#each items as { id, name }, i}
    <div>{i}: {id} - {name}</div>
  {/each}
{/snippet}

<main>
  {@render renderItems(allItems)}
  {@render itemList(allItems, "Items")}
</main>
