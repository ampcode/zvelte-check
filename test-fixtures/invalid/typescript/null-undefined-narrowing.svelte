<script lang="ts">
  // Test case for null vs undefined type narrowing
  // svelte-check should catch that null is not assignable to undefined
  
  type ServerStatus = { connected: boolean };
  
  // Function that only accepts undefined, not null
  function isEnabled(config: any, status: ServerStatus | undefined): boolean {
    return status?.connected ?? false;
  }
  
  // This prop can be null, undefined, or ServerStatus
  let { serverStatus }: { serverStatus: ServerStatus | null | undefined } = $props();
  
  // Error: null is not assignable to undefined
  let enabled = $derived(isEnabled({}, serverStatus));
</script>

<p>Enabled: {enabled}</p>
