<script lang="ts">
	// Test: ternary expressions in arrow function bodies should preserve narrowing.
	// The truthy branch of `obj?.data ?` should narrow obj to be defined.
	
	import Comp from './Comp.svelte';
	
	let obj: { data?: { id: string } } | undefined = $state();
</script>

<!-- The getter arrow function contains a ternary. Inside the truthy branch,
     obj should be narrowed to non-undefined. This was a false positive:
     zvelte-check reported "'obj' is possibly 'undefined'" on obj.data.id -->
<Comp
	value={
		() => obj?.data
			? { id: obj.data.id, value: obj }
			: undefined,
		(v) => { obj = v?.value }
	}
/>
