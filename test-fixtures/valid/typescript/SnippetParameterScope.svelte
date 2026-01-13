<!--
  Test that snippet parameters are properly scoped.
  This is a regression test for the fix that ensures expressions inside
  snippet bodies have access to snippet parameters.
  
  Previously, expressions like {@render inner(item)} inside a snippet
  would cause "Cannot find name 'item'" errors because the parameter
  was not in scope at module level.
-->
<script lang="ts">
    interface Item {
        id: string;
        name: string;
    }
    
    let items: Item[] = $state([]);
    
    function processItem(item: Item) {
        console.log(item.id);
    }
</script>

<div class="container">
    <!-- Inner snippet to be called from outer snippets -->
    {#snippet inner(item: Item)}
        <span>{item.name}</span>
    {/snippet}
    
    <!-- Outer snippet that uses the parameter in @render -->
    {#snippet outer(item: Item)}
        <div class="row">
            {@render inner(item)}
            <button onclick={() => processItem(item)}>
                Process {item.id}
            </button>
        </div>
    {/snippet}
    
    <!-- Use the snippets -->
    {#each items as item}
        {@render outer(item)}
    {/each}
</div>
