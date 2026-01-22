<script lang="ts">
	type ContentBlock = { type: 'text'; text: string } | { type: 'image'; url: string };
	type Message = { role: 'user'; content: ContentBlock[] } | { role: 'assistant'; content: ContentBlock[] };
	let { message }: { message: Message } = $props();
</script>

{#if message.role === 'user'}
	{#each message.content as block}
		{#if block.type === 'text'}
			<p>User: {block.text}</p>
		{/if}
	{/each}
{:else if message.role === 'assistant'}
	{#each message.content as block}
		{#if block.type === 'text'}
			<p>Assistant: {block.text}</p>
		{/if}
	{/each}
{/if}
