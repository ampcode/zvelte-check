<script lang="ts" generics="T">
	interface Props {
		items: T[];
		selected?: T;
	}
	let { items, selected = $bindable() }: Props = $props();
</script>

{#each items as item}
	<button onclick={() => selected = item}>
		{item}
	</button>
{/each}
