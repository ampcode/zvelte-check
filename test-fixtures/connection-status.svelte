<script lang="ts">
    import MCPServerError from './MCPServerError.svelte'
    
    type ConnectionStatus =
        | { type: 'awaiting-approval' }
        | { type: 'connecting' }
        | { type: 'connected', capabilities: object }
        | { type: 'failed'; error: Error }
        | { type: 'blocked-by-registry'; registryUrl: string }
    
    type Server = { name: string, status: ConnectionStatus }
    let userServers: Server[] = $state([])
</script>

{#each userServers as server}
    {#if server.status.type === 'failed'}
        <MCPServerError error={server.status.error} />
    {:else if server.status.type === 'blocked-by-registry'}
        <div>{server.status.registryUrl}</div>
    {/if}
{/each}
