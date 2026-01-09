<script>
    // Test that a11y checks don't run on components (only HTML elements)
    // Props like autofocus passed to components shouldn't trigger warnings
    let autofocus = true;
</script>

<!-- This is a component (PascalCase) - no a11y warnings expected -->
<MyInput {autofocus} />

<!-- This is an HTML element - a11y_autofocus warning expected for static autofocus -->
<!-- svelte-ignore a11y_autofocus -->
<input autofocus />
