<script lang="ts">
	interface Props {
		onsubmit?: (content: string) => void;
	}
	let { onsubmit }: Props = $props();
</script>

<div>
	<button onclick={() => onsubmit?.('test')}>Submit</button>
</div>
