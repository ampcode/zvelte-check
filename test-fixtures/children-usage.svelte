<script lang="ts">
    import ChildrenSnippet from './children-snippet.svelte';
</script>

<!-- This has children - should pass children prop automatically -->
<ChildrenSnippet label="Hello">
    Some text content between tags
</ChildrenSnippet>

<!-- Self-closing - no children -->
<ChildrenSnippet label="World" />
