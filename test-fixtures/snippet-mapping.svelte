<script lang="ts">
    let currentAd = $state<{id: string, name: string} | null>(null);
</script>

{#snippet ribbonAdContent()}
    <div>
        {currentAd.id}
        {currentAd.name}
    </div>
{/snippet}

{#if currentAd}
    {@render ribbonAdContent()}
{/if}
