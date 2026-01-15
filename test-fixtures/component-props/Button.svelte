<script lang="ts">
	interface Props {
		label: string;
		onclick?: () => void;
	}
	let { label, onclick }: Props = $props();
</script>

<button onclick={onclick}>{label}</button>
