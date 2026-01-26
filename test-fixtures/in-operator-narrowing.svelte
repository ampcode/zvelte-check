<script lang="ts">
	// Regression test: 'in' operator type narrowing for union types
	// https://github.com/ampcode/zvelte-check/issues/2299
	// 
	// When a snippet is defined inside {#if condition} that narrows a union type,
	// the 'in' operator should work without errors because TypeScript knows the
	// string member of the union has been excluded.

	type Status = { ok: true; data: string } | { error: { message: string } } | 'pending'
	
	let status: Status = $state('pending')
</script>

<!-- Regression test: the snippet is defined inside an {#if} that narrows the type,
     so the 'in' operator check should work without errors. -->
{#if status !== 'pending'}
	{#snippet statusContent()}
		{#if 'error' in status}
			<p>Error: {status.error.message}</p>
		{:else}
			<p>OK: {status.data}</p>
		{/if}
	{/snippet}
	{@render statusContent()}
{/if}
