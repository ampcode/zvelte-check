<!--
  Valid component demonstrating Svelte 5 snippets.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Item {
        id: number;
        title: string;
        description: string;
    }

    interface Props {
        items: Item[];
        header?: Snippet;
        footer?: Snippet<[{ count: number }]>;
        itemTemplate?: Snippet<[Item, number]>;
        empty?: Snippet;
    }

    let { items, header, footer, itemTemplate, empty }: Props = $props();

    let count = $derived(items.length);
</script>

{#snippet defaultItem(item: Item, index: number)}
    <div class="item">
        <span class="index">{index + 1}.</span>
        <strong>{item.title}</strong>
        <p>{item.description}</p>
    </div>
{/snippet}

{#snippet defaultEmpty()}
    <p class="empty">No items to display.</p>
{/snippet}

<div class="list-container">
    {#if header}
        <header>
            {@render header()}
        </header>
    {/if}

    <div class="items">
        {#if items.length === 0}
            {#if empty}
                {@render empty()}
            {:else}
                {@render defaultEmpty()}
            {/if}
        {:else}
            {#each items as item, index (item.id)}
                {#if itemTemplate}
                    {@render itemTemplate(item, index)}
                {:else}
                    {@render defaultItem(item, index)}
                {/if}
            {/each}
        {/if}
    </div>

    {#if footer}
        <footer>
            {@render footer({ count })}
        </footer>
    {/if}
</div>

<style>
    .list-container {
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        overflow: hidden;
    }

    header,
    footer {
        padding: 0.75rem 1rem;
        background: #f5f5f5;
    }

    .items {
        padding: 1rem;
    }

    .item {
        padding: 0.5rem 0;
        border-bottom: 1px solid #eee;
    }

    .item:last-child {
        border-bottom: none;
    }

    .index {
        color: #888;
        margin-right: 0.5rem;
    }

    .empty {
        color: #888;
        text-align: center;
        padding: 2rem;
    }
</style>
