<!--
  Regression test: property named 'type' in $props() destructuring.
  
  `type = 'button'` is a destructured prop with default value, NOT a TypeScript type alias.
  The transformer's separateImports() was incorrectly treating lines starting with "type "
  as type alias declarations.
  
  See: task 2239
-->
<script lang="ts">
	interface ButtonProps {
		type?: 'button' | 'submit' | 'reset'
		name?: string
	}

	let {
		type = 'button',
		name = 'test',
	}: ButtonProps = $props()
</script>

<button {type}>
	{name}
</button>
