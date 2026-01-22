<script lang="ts">
	// Test case: snippet inside {#if} and {:else if} should preserve narrowing context
	let { onAction, onCopy }: { onAction?: () => void; onCopy?: () => void } = $props();
</script>

<!-- Case 1: Simple {#if} - this should work -->
{#if onAction}
	<!-- Direct call works - narrowed to non-undefined -->
	<button onclick={() => onAction()}>Direct</button>
	
	<!-- Snippet inside if-block loses narrowing -->
	{#snippet child()}
		<button onclick={() => onAction()}>Via Snippet</button>
	{/snippet}
	{@render child()}
{/if}

<!-- Case 2: {:else if} - this is the actual bug from code-review-comments.svelte -->
{#if onAction}
	<button onclick={() => onAction()}>Action</button>
{:else if onCopy}
	<!-- Direct call works - narrowed to non-undefined -->
	<button onclick={() => onCopy()}>Direct Copy</button>
	
	<!-- Snippet inside {:else if} block - tests narrowing propagation -->
	{#snippet copyChild()}
		<button onclick={() => onCopy()}>Via Snippet in else-if</button>
	{/snippet}
	{@render copyChild()}
{/if}
