<script lang="ts">
	interface Props {
		name: string;
		count?: number;
	}
	let { name, count = 0 }: Props = $props();
</script>

<p>Hello {name}! Count: {count}</p>
