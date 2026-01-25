<script lang="ts">
    let count = $state(0)
    
    // Without svelte-ignore - should warn
    const capturedWithoutIgnore = count
    
    // svelte-ignore state_referenced_locally
    const capturedWithIgnore = count
    
    /* svelte-ignore state_referenced_locally */
    const capturedWithBlockComment = count
    
    // svelte-ignore other_code
    const capturedWithWrongCode = count
</script>

<p>Count: {count}</p>
