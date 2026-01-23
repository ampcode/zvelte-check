<script lang="ts">
    let value: string | null = $state(null);
</script>

{#snippet showValue()}
    <span>{value.length}</span>
{/snippet}

{#if value}
    {@render showValue()}
{/if}
