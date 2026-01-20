<script lang="ts">
  let items = [{ name: 'test', value: 42 }];
</script>

{#each items as item, index}
  {#snippet detail()}
    <div>{item.name}</div>
    <span>{item.value}</span>
    <p>Index: {index}</p>
  {/snippet}
  
  {@render detail()}
{/each}
