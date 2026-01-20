<script lang="ts">
// Regression test: {@const} inside {#each} must be emitted before its usages.
// Otherwise TypeScript reports "Cannot find name" errors.
interface Item {
  name: string;
  value: number;
}
let items: Item[] = [{ name: 'a', value: 1 }, { name: 'b', value: 2 }];
</script>

{#each items as item}
  {@const name = item.name}
  {@const doubled = item.value * 2}
  <div>{name}: {doubled}</div>
{/each}
