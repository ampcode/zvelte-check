<script lang="ts">
    import MCPServerError from './MCPServerError.svelte'
    type Status = { type: 'ok' } | { type: 'failed', error: string }
    type Server = { name: string, status: Status }
    let userServers: Server[] = $state([])
</script>

{#each userServers as server}
    {#if server.status.type === 'failed'}
        <MCPServerError error={server.status.error} />
    {/if}
{/each}
