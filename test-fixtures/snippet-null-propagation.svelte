<script lang="ts">
	// Regression test: null check propagation into snippet IIFEs
	// https://github.com/ampcode/zvelte-check/issues/2300
	//
	// When a snippet is defined inside {#if value} that narrows a nullable type,
	// expressions inside the snippet should see the narrowed type.

	type Usage = { maxTokens: number; current: number }
	let usage: Usage | null | undefined = $state(null)
</script>

{#if usage}
	{#snippet stats()}
		<!-- usage should be narrowed to Usage here, not Usage | null | undefined -->
		<span>Used: {usage.current} of {usage.maxTokens}</span>
	{/snippet}
	{@render stats()}
{/if}
