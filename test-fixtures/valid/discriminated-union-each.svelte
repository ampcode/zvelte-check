<script lang="ts">
  type TextBlock = { type: 'text'; text: string };
  type ToolUseBlock = { type: 'tool_use'; tool: string };
  type Block = TextBlock | ToolUseBlock;

  type Message = { role: 'user'; query: string } | { role: 'assistant'; content: Block[] };
  
  let message: Message = $state({ role: 'user', query: 'hello' });
</script>

{#if message.role === 'user'}
  <p>{message.query}</p>
{:else if message.role === 'assistant'}
  {#each message.content as block}
    {@const isTool = block.type === 'tool_use'}
    {#if block.type === 'text'}
      <p>{block.text}</p>
    {:else if block.type === 'tool_use'}
      <p>{block.tool}</p>
    {/if}
  {/each}
{/if}
