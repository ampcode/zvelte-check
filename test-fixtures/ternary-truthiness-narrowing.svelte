<script lang="ts">
	import type { Snippet } from 'svelte';
	import MessageStep from './Nested/MessageStep.svelte';
	
	interface Props {
		handleSubmitUserMessage?: (content: string, index: number) => void;
		inputDisabled?: boolean;
		lastSummaryIndex?: number;
		children?: Snippet;
	}
	let { handleSubmitUserMessage, inputDisabled, lastSummaryIndex = 0, children }: Props = $props();
</script>

<!--
  Regression test for truthiness narrowing with ternary expressions.
  
  When `isEditable = !!(handleSubmitUserMessage && ...)`, the truthy branch
  of `isEditable ? ... : ...` should narrow `handleSubmitUserMessage` to defined.
  
  Two mechanisms work together to enable this:
  1. `{@const}` bindings emit as `const` (not `var`) for TypeScript closure
     narrowing to work. TypeScript only narrows immutable bindings in closures.
  2. `emitClosureBodyExpressionsForAttrs` extracts member accesses from closures
     and emits void statements before components, providing explicit narrowing hints.
  
  This pattern is common in Svelte when callbacks are conditionally available.
  
  This test reproduces the pattern from amp2/web thread.svelte where:
  - handleSubmitUserMessage is an optional callback prop
  - isEditable is computed from handleSubmitUserMessage being defined
  - The callback is invoked inside a ternary guarded by isEditable
  
  Before fix: "Cannot invoke an object which is possibly 'undefined'"
  After fix: No error
-->

{#snippet userMessage(messageIndex: number)}
	{@const isEditable = !!(
		handleSubmitUserMessage &&
		messageIndex > lastSummaryIndex &&
		!inputDisabled
	)}
	
	<!-- 
	  Inside the truthy branch of the ternary, handleSubmitUserMessage should
	  be narrowed to defined because isEditable being true implies it exists.
	  
	  Before fix (with `var`): "Cannot invoke an object which is possibly 'undefined'"
	  After fix (with `const`): No error (TypeScript closure narrowing works)
	-->
	<MessageStep
		onsubmit={isEditable
			? (content: string) => handleSubmitUserMessage(content, messageIndex)
			: undefined}
	/>
{/snippet}

{@render userMessage(0)}
{@render userMessage(1)}
