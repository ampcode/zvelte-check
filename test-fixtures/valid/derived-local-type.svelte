<script lang="ts">
  let { items }: { items: string[] } = $props()

  const sorted = $derived.by(() => {
    type Item = (typeof items)[number]
    const compare = (a: Item, b: Item) => a.localeCompare(b)
    return [...items].sort(compare)
  })
</script>

<ul>
  {#each sorted as item}
    <li>{item}</li>
  {/each}
</ul>
