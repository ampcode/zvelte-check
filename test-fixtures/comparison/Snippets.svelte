<script lang="ts">
	import type { Snippet } from 'svelte';

	interface Props {
		header: Snippet;
		children: Snippet;
	}
	let { header, children }: Props = $props();
</script>

{@render header()}
<div class="content">
	{@render children()}
</div>
