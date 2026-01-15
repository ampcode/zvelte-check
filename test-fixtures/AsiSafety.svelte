<script lang="ts">
	// Test case for ASI (Automatic Semicolon Insertion) safety.
	// When script content doesn't end with a semicolon, the template
	// expressions IIFE must not be parsed as a function call on the
	// previous expression.
	
	// This pattern caused TS2349: "This expression is not callable"
	// because TypeScript parsed it as: $derived(expr)((...) => {...})(undefined!)
	type VersionInfo = { sha?: string; age?: string }
	function parseVersionInfo(v: string): VersionInfo { return {} }
	
	let { clientVersion = '' } = $props()
	
	// No semicolon after this line - tests ASI safety
	let versionInfo = $derived(clientVersion ? parseVersionInfo(clientVersion) : null)
</script>

{#snippet child({ props }: { props: Record<string, unknown> })}
	<span {...props}>{versionInfo?.age}</span>
{/snippet}

{@render child({ props: {} })}
