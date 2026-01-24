<!--
  Regression test: Rest patterns in snippet parameters.
  
  When a snippet parameter uses object destructuring with rest spread,
  the transformer's placeholder arguments must be object types (not undefined!)
  because TypeScript requires "Rest types may only be created from object types".
  
  This tests that the IIFE placeholder uses {} as any instead of undefined!
  so TypeScript can apply rest spread patterns in destructuring.
  
  See: task 2240
-->
<script lang="ts">
	interface TriggerState {
		props: {
			triggerClass: string
			role: string
			'aria-expanded': boolean
		}
	}
</script>

<!-- Snippet with rest pattern in parameter - requires object type placeholder -->
{#snippet child({ props: { triggerClass, ...triggerProps } }: TriggerState)}
	<button class={triggerClass} {...triggerProps}>Click me</button>
{/snippet}

<div>
	{@render child({ props: { triggerClass: 'test', role: 'button', 'aria-expanded': false } })}
</div>
