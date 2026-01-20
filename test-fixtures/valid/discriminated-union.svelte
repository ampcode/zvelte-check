<script lang="ts">
  type TextBlock = { type: 'text'; text: string };
  type ThinkingBlock = { type: 'thinking'; thought: string };
  type ToolUseBlock = { type: 'tool_use'; tool: string };
  type Block = TextBlock | ThinkingBlock | ToolUseBlock;

  let block: Block = $state({ type: 'text', text: 'hello' });
</script>

{#if block.type === 'text'}
  <p>{block.text}</p>
{:else if block.type === 'thinking'}
  <p>{block.thought}</p>
{:else if block.type === 'tool_use'}
  <p>{block.tool}</p>
{/if}
