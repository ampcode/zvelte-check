<!--
  Generic component with multiple type parameters.
  Tests complex generics syntax.
  Should produce zero diagnostics.
-->
<script lang="ts" generics="TKey extends string | number, TValue">
    interface Props {
        key: TKey;
        value: TValue;
        items: Map<TKey, TValue>;
        onSelect?: (key: TKey, value: TValue) => void;
    }
    
    let { key, value, items, onSelect }: Props = $props();
    
    let selectedKey = $state<TKey | null>(null);
    
    function handleSelect(k: TKey) {
        const v = items.get(k);
        if (v !== undefined) {
            selectedKey = k;
            onSelect?.(k, v);
        }
    }
</script>

<div class="selector">
    <p>Current: {key} = {JSON.stringify(value)}</p>
    
    {#each [...items.entries()] as [k, v]}
        <button 
            type="button"
            onclick={() => handleSelect(k)}
            class:selected={selectedKey === k}
        >
            {k}
        </button>
    {/each}
</div>

<style>
    .selector {
        display: flex;
        gap: 0.5rem;
        flex-wrap: wrap;
    }
    
    button {
        padding: 0.5rem 1rem;
        border: 1px solid #ccc;
        border-radius: 4px;
        cursor: pointer;
    }
    
    .selected {
        background: #007bff;
        color: white;
        border-color: #007bff;
    }
</style>
