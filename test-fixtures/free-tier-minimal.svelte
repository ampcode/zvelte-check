<script lang="ts">
	function observableToReadable<T>(): {subscribe: () => T} {
		return {subscribe: () => undefined as T}
	}
	let currentAd = $derived(observableToReadable<{id: string, name: string} | undefined>())
</script>

{#snippet ribbonAdContent()}
	<div id={$currentAd.id}>{$currentAd.name}</div>
{/snippet}

{#if $currentAd}
	{@render ribbonAdContent()}
{/if}
