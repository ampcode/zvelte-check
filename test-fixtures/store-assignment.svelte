<!--
  Regression test for Svelte 4 store assignment syntax.
  $store = value should compile to store.set(value), not be treated as const assignment.
  
  This was a false positive where zvelte-check reported:
  "Cannot assign to '$status' because it is a constant"
  
  See: task 2238
-->
<script lang="ts">
	import { writable, type Writable } from 'svelte/store'

	type Status = 'idle' | 'hover' | 'drag'

	// Store declared with const (the store itself is const, its value is not)
	const status: Writable<Status> = writable('idle')

	// Store declared with let
	let otherStore: Writable<number> = writable(0)

	function onMouseEnter() {
		// Store assignment should work even though store is declared with const
		$status = 'hover'
	}

	function onMouseDown() {
		$status = 'drag'
		$otherStore = 1
	}

	function onMouseUp() {
		if ($status === 'drag') {
			$status = 'hover'
		}
	}

	function reset() {
		$status = 'idle'
		$otherStore = 0
	}
</script>

<button on:click={reset}>
	Status: {$status}, Other: {$otherStore}
</button>
