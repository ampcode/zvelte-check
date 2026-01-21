<script lang="ts">
  async function getClipboardContent(): Promise<string> {
    return "content";
  }
  
  async function getUser(): Promise<{ name: string; id: number }> {
    return { name: "test", id: 1 };
  }
</script>

<!-- Simple then binding -->
{#await getClipboardContent() then clipboard}
  <div>{clipboard.toUpperCase()}</div>
{/await}

<!-- Destructuring then binding -->
{#await getUser() then { name, id }}
  <div>{name}: {id}</div>
{/await}
