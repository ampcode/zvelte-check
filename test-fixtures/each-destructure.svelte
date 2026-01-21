<script lang="ts">
  // Test: Destructuring in {#each} should not trigger "All destructured elements are unused"
  const items: Array<[string, { title: string }]> = [];
</script>

{#each items as [id, item]}
  <div class="entry">{id}: {item.title}</div>
{/each}
