<!-- Regression test for bug where apostrophe in JS comment inside component
     attribute expression caused false "Cannot find name" errors for text content.
     
     The bug was in findMatchingCloseBrace and extractIdentifiersFromAttributeValue
     which didn't handle JS comments, causing apostrophes like "it's" to be
     misinterpreted as string delimiters, corrupting brace matching.
-->
<script lang="ts">
import Comp from './comp.svelte';
const items = [];
</script>

<section>
	<Comp prop={{
		x: [
			// it's a comment with apostrophe
			{ y: 1 },
		],
	}} />
</section>

<section>
	<h3>Text content here</h3>
</section>
