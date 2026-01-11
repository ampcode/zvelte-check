<script lang="ts">
	interface Props {
		value: string;
	}
	let { value = $bindable() }: Props = $props();
</script>

<input bind:value />
