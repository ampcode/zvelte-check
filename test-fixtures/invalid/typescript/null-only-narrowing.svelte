<script lang="ts">
  // Test case for null-only type (no undefined)
  // This tests the filter at line 982
  
  type ServerStatus = { connected: boolean };
  
  // Function that takes ServerStatus only (no null, no undefined)
  function requireStatus(status: ServerStatus): boolean {
    return status.connected;
  }
  
  // This prop can be null or ServerStatus (NOT undefined)
  let { serverStatus }: { serverStatus: ServerStatus | null } = $props();
  
  // Error: null is not assignable to ServerStatus
  // Message: "Argument of type 'ServerStatus | null' is not assignable to parameter of type 'ServerStatus'"
  let result = $derived(requireStatus(serverStatus));
</script>

<p>Result: {result}</p>
