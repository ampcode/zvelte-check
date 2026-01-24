<script lang="ts">
	// Test: $state with explicit type argument is NOT a circular reference
	// svelte-check 4.3.5 (with svelte2tsx 0.7.42) incorrectly reports errors for this
	// pattern due to a version mismatch with Svelte 5. zvelte-check correctly
	// accepts this valid Svelte 5 code.
	//
	// The type argument provides the explicit type, so TypeScript doesn't need
	// to infer the type from the initializer - no circular dependency exists.

	interface SimulationState {
		count: number
	}

	function createInitialSimulationState(): SimulationState {
		return { count: 0 }
	}

	// This is VALID - SimulationState is explicitly specified via type argument
	let state = $state<SimulationState>(createInitialSimulationState())

	// Alternative: explicit type annotation (also valid, more verbose)
	let state2: SimulationState = $state(createInitialSimulationState())
</script>

<p>{state.count} {state2.count}</p>
