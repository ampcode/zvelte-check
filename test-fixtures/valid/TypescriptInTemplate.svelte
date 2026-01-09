<!--
  TypeScript type annotations in template expressions.
  Tests parser handling of 'as' casts and type annotations in control flow.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Item {
        id: number;
        name: string;
    }
    
    let value: unknown = $state("hello");
    let items: Item[] = $state([]);
    let promise: Promise<string> = Promise.resolve("done");
</script>

<!-- Type assertions in if blocks -->
{#if value as boolean}
    <p>Value is truthy</p>
{:else if (value as string).length > 0}
    <p>Value is non-empty string</p>
{/if}

<!-- Type annotations in each blocks -->
{#each items as item: Item, i: number (item.id)}
    <p>{item.name} at index {i}</p>
{/each}

<!-- Type annotations in await blocks -->
{#await promise as Promise<string>}
    <p>Loading...</p>
{:then result: string}
    <p>Result: {result}</p>
{:catch error: Error}
    <p>Error: {error.message}</p>
{/await}

<!-- Type annotations in key blocks -->
{#key value as string}
    <p>Key changed</p>
{/key}

<!-- Event handlers with typed parameters -->
<button onclick={(e: MouseEvent) => console.log(e.clientX)}>
    Click me
</button>

<!-- Attribute with type assertion -->
<div data-value={value as string}>
    Content
</div>

<!-- Non-null assertion -->
<label id={items[0]?.id!.toString()}>Label</label>
