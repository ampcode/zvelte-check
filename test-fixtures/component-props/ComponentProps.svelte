<script lang="ts">
	import Button from './Button.svelte';
	import * as Table from './Table';

	let name = $state('world');
	let count = $state(0);
</script>

<!-- Simple component with valid prop -->
<Button label="Click me" />

<!-- Component with invalid prop (should error) -->
<Button invalidProp="test" />

<!-- Namespace component with valid prop -->
<Table.Row id="row-1" />

<!-- Namespace component with invalid prop (should error) -->
<Table.Row unknownProp={123} />

<!-- Component with expression props -->
<Button label={name} onclick={() => count++} />
