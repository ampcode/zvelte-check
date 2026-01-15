<script lang="ts">
	import type { Snippet } from 'svelte';

	interface Props {
		id: string;
		children?: Snippet;
	}
	let { id, children }: Props = $props();
</script>

<tr id={id}>
	{@render children?.()}
</tr>
