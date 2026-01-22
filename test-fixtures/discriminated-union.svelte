<script lang="ts">
    type Status = { type: 'ok' } | { type: 'failed', error: string }
    let status: Status = $state({ type: 'ok' })
</script>

{#if status.type === 'failed'}
    <div>{status.error}</div>
{/if}
