<!--
Regression test for arrow function event handlers with comments containing
apostrophes and braces. Without proper comment handling in isSingleExpression(),
the apostrophe in "Don't" would be parsed as a string delimiter, causing
cascading parse errors.
-->
<script lang="ts">
	let visible = $state(false)
</script>

<div
	onblur={(e) => {
		// Don't close menus if visible - line comment with apostrophe
		if (!visible) {
			setTimeout(() => {
				visible = false
			}, 200)
		}
	}}
	onfocus={(e) => {
		/* Block comment with apostrophes: don't, won't, can't */
		visible = true
	}}
>
	Content
</div>
