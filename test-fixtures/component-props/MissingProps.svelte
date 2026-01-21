<script lang="ts">
	import Button from './Button.svelte';
	
	// Button requires label: string prop
</script>

<!-- Missing required prop 'label' - should error -->
<Button />

<!-- Only optional prop provided - still missing 'label' - should error -->
<Button onclick={() => console.log('clicked')} />
