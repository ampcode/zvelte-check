<script lang="ts">
	// Test that store subscriptions don't break multi-line ternary expressions
	interface Store<T> {
		subscribe: (fn: (v: T) => void) => void
	}
	
	function createStore(): Store<string> | undefined {
		return undefined
	}
	
	const maybeStore = createStore()
	const storeValue = maybeStore
		? maybeStore
		: undefined
	
	// Using $storeValue should work
	const value = $storeValue ?? 'default'
</script>

<p>{value}</p>
