<script lang="ts">
	// Regression test: IIFE inside {#if} should not trigger "function always defined" error
	// When we have {#each} inside {#if}, we generate an IIFE for the each binding.
	// The narrowing conditions should be emitted INSIDE the IIFE, not both inside and outside.
	let { onClick }: { onClick?: () => void } = $props();
	let items = ['a', 'b', 'c'];
</script>

{#if onClick}
	<p>Direct call works:</p>
	<button onclick={() => onClick()}>Click me</button>
	
	{#each items as item}
		<!-- This should NOT trigger "This condition will always return true" -->
		<button onclick={() => onClick()}>{item}</button>
	{/each}
{/if}
