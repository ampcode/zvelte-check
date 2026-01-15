<script lang="ts">
	type Status = 'pending' | 'success' | 'error';
	
	let {
		maybeDate,
		maybeString,
		status,
	}: {
		maybeDate?: Date;
		maybeString?: string;
		status: Status;
	} = $props();
	
	function formatDate(date: Date): string {
		return date.toISOString();
	}
	
	function processString(s: string): string {
		return s.toUpperCase();
	}
</script>

<!-- Basic narrowing: maybeDate is Date | undefined, should be Date inside if block -->
{#if maybeDate}
	<span>{formatDate(maybeDate)}</span>
{/if}

<!-- Else branch: maybeString should be undefined inside else -->
{#if maybeString}
	<span>{processString(maybeString)}</span>
{:else}
	<span>No string provided</span>
{/if}

<!-- Discriminated union narrowing -->
{#if status === 'success'}
	<span>Success!</span>
{:else if status === 'error'}
	<span>Error occurred</span>
{:else}
	<span>Pending...</span>
{/if}

<!-- Nested if blocks -->
{#if maybeDate}
	{#if maybeString}
		<span>{formatDate(maybeDate)} - {processString(maybeString)}</span>
	{/if}
{/if}
