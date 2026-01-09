<!--
  @const tag usage in various contexts.
  Tests proper scoping of @const declarations.
  Should produce zero diagnostics (for valid cases).
-->
<script lang="ts">
    interface Item {
        id: number;
        value: number;
    }
    
    let items: Item[] = $state([
        { id: 1, value: 10 },
        { id: 2, value: 20 },
        { id: 3, value: 30 }
    ]);
    
    let promise = Promise.resolve({ data: 'hello' });
</script>

<!-- @const in each block -->
{#each items as item}
    {@const doubled = item.value * 2}
    {@const label = `Item ${item.id}`}
    <p>{label}: {doubled}</p>
{/each}

<!-- @const with destructuring -->
{#each items as { id, value }}
    {@const computed = id + value}
    <p>Computed: {computed}</p>
{/each}

<!-- @const in await block -->
{#await promise then result}
    {@const upperData = result.data.toUpperCase()}
    <p>Data: {upperData}</p>
{:catch error}
    {@const message = error.message || 'Unknown error'}
    <p>Error: {message}</p>
{/await}

<!-- @const in if block -->
{#if items.length > 0}
    {@const first = items[0]}
    {@const last = items[items.length - 1]}
    <p>First: {first.value}, Last: {last.value}</p>
{/if}

<!-- Nested @const declarations -->
{#each items as item}
    {@const base = item.value}
    {#if base > 10}
        {@const adjusted = base - 10}
        <p>Adjusted: {adjusted}</p>
    {/if}
{/each}

<!-- @const shadowing outer variables -->
{#each items as item}
    {@const items = [item]}
    <p>Shadowed: {items.length}</p>
{/each}
