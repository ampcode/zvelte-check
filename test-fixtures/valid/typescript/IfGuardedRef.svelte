<script lang="ts">
	// Test: if-guarded refs inside closures should not trigger "possibly null" errors
	//
	// Pattern from thread-list-label-filter.svelte. When a nullable ref is accessed
	// inside a closure, and the access is guarded by an if-statement, we should not
	// emit `void (ref.value)` for template narrowing - the if-guard already handles it.
	//
	// Without this fix, zvelte-check would extract member accesses from closure bodies
	// and emit them at module scope for template-level narrowing, but this caused false
	// positives like "'searchInputRef' is possibly 'null'" even when the code had an
	// explicit if-guard.
	import { tick } from 'svelte'

	let searchInputRef = $state<HTMLInputElement | null>(null)

	function handleChange() {
		// After async operation, check and access ref
		Promise.resolve().then(() => {
			if (searchInputRef) searchInputRef.value = ''
		})
	}

	async function handleOpen() {
		await Promise.resolve()
		if (searchInputRef) {
			searchInputRef.value = ''
		}
	}
</script>

<input bind:this={searchInputRef} />
<!-- Inline callback in template - this is where the issue happens -->
<button onclick={(e) => {
	tick().then(() => {
		if (searchInputRef) searchInputRef.value = ''
	})
}}>Change</button>
<button onclick={async () => {
	await tick()
	if (searchInputRef) {
		searchInputRef.value = ''
	}
}}>Open</button>
