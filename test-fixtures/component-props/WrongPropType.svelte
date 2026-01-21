<script lang="ts">
	import Button from './Button.svelte';
	
	// Button expects label: string, onclick?: () => void
</script>

<!-- Wrong type for 'label' - should error -->
<Button label={123} />

<!-- Wrong type for 'onclick' - should error -->
<Button label="ok" onclick="not a function" />
