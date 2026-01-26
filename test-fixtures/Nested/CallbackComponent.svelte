<script lang="ts">
	interface Props {
		onSearchChange?: (query: string) => void;
		onSelect?: (item: { id: number; name: string }) => void;
	}
	let { onSearchChange, onSelect }: Props = $props();
</script>

<div>
	<input oninput={(e) => onSearchChange?.(e.currentTarget.value)} />
	<button onclick={() => onSelect?.({ id: 1, name: 'test' })}>Select</button>
</div>
