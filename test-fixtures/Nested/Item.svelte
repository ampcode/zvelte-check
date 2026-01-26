<script lang="ts">
	import type { HTMLButtonAttributes } from 'svelte/elements';
	import type { Snippet } from 'svelte';
	
	let {
		children,
		...restProps
	}: HTMLButtonAttributes & {
		children?: Snippet;
	} = $props();
</script>

<button {...restProps}>
	{@render children?.()}
</button>
