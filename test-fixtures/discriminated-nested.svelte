<script lang="ts">
    import MCPServerError from './MCPServerError.svelte'
    type Status = { type: 'ok' } | { type: 'failed', error: string }
    type Server = { name: string, status: Status }
    let server: Server = $state({ name: 'test', status: { type: 'ok' } })
</script>

{#if server.status.type === 'failed'}
    <MCPServerError error={server.status.error} />
{/if}
