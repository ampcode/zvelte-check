<script lang="ts">
// Test case for callback parameter type inference in template expressions
// https://github.com/ampcode/zvelte-check/issues/XXX
interface Item {
  id: number;
  name: string;
  isDefault: boolean;
}
let items: Item[] = [];

// Script callback - should infer types from array
const sorted = items.toSorted((a, b) => a.id - b.id);
</script>

<!-- Template callback - should also infer types, not report implicit any -->
{#each items.toSorted((a, b) => a.id - b.id) as item}
  <p>{item.name}</p>
{/each}
