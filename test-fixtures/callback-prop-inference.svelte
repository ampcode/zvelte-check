<script lang="ts">
	import CallbackComponent from './Nested/CallbackComponent.svelte';
	
	let currentSearchQuery = $state('');
	let selectedItem = $state<{ id: number; name: string } | null>(null);
	
	// Explicitly test that the callback parameter types are inferred correctly
	// by calling string methods on query and accessing object properties on item
</script>

<!--
  Regression test for callback parameter type inference from component props.
  
  When a component has typed callback props like `onSearchChange?: (query: string) => void`,
  inline arrow functions passed to that prop should have their parameter types inferred.
  
  Example:
    <CallbackComponent onSearchChange={(query) => ...} />
    
  The `query` parameter should be inferred as `string` from the prop type.
  
  Before fix: "Parameter 'query' implicitly has an 'any' type"
  After fix: No error (TypeScript infers type from prop context)
-->

<!-- Simple string callback - parameter should be inferred as string -->
<CallbackComponent
	onSearchChange={(query) => (currentSearchQuery = query)}
/>

<!-- Object callback - parameter should be inferred as { id: number; name: string } -->
<CallbackComponent
	onSelect={(item) => (selectedItem = item)}
/>

<!-- Both callbacks together -->
<CallbackComponent
	onSearchChange={(q) => console.log(q.toUpperCase())}
	onSelect={(i) => console.log(i.id, i.name)}
/>
