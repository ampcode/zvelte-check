<!--
  Svelte 5 error boundary syntax.
  Tests parser handling of svelte:boundary.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import RiskyComponent from './RiskyComponent.svelte';
    
    function handleError(error: Error) {
        console.error('Caught error:', error);
    }
</script>

<!-- Basic error boundary -->
<svelte:boundary onerror={handleError}>
    <RiskyComponent />
</svelte:boundary>

<!-- Error boundary with failed snippet -->
<svelte:boundary onerror={(e) => console.error(e)}>
    <RiskyComponent />
    {#snippet failed(error)}
        <div class="error">
            <h2>Something went wrong</h2>
            <p>{error.message}</p>
        </div>
    {/snippet}
</svelte:boundary>

<!-- Nested boundaries -->
<svelte:boundary onerror={handleError}>
    <div>
        <svelte:boundary onerror={(e) => console.log('Inner:', e)}>
            <RiskyComponent />
            {#snippet failed(e)}
                <p>Inner error: {e.message}</p>
            {/snippet}
        </svelte:boundary>
    </div>
    {#snippet failed(e)}
        <p>Outer error: {e.message}</p>
    {/snippet}
</svelte:boundary>

<style>
    .error {
        padding: 1rem;
        background: #fee;
        border: 1px solid #f00;
        border-radius: 4px;
    }
</style>
