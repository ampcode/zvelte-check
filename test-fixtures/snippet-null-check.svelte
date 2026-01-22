<script lang="ts">
    let currentAd: {id: string, name: string} | undefined = $state();
</script>

{#snippet ribbonAdContent()}
    <div>{currentAd.id}</div>
{/snippet}

{#if currentAd}
    {@render ribbonAdContent()}
{/if}
