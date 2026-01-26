<script lang="ts">
	import * as ContextMenu from './Nested';
</script>

<!--
  Regression test for control-flow type narrowing in {#if} blocks with closures.
  
  TypeScript only narrows types inside closures for immutable (const) bindings,
  not mutable (let/var) bindings. The transformer must emit `const` for {@const}
  bindings to enable this narrowing.
  
  Before fix: `var codeBlock = ...` → Error: 'codeBlock' is possibly 'null'
  After fix: `const codeBlock = ...` → No error (narrowing works in closure)
-->
{#snippet copyContextItems(target: HTMLElement)}
	{@const codeBlock = target.closest('pre')}

	{#if codeBlock}
		<ContextMenu.Item
			onclick={() => navigator.clipboard.writeText(codeBlock.textContent ?? '')}
		>
			Copy Code
		</ContextMenu.Item>
	{/if}
{/snippet}

{@render copyContextItems(document.body)}
