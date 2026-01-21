<script lang="ts">
    import NamedSnippetOnly from './named-snippet-only.svelte';
</script>

<!-- Only has a named snippet - should NOT pass children -->
<NamedSnippetOnly icon="test" tooltip="Hello">
    {#snippet openTooltipSnippet()}
        <span>Custom tooltip content</span>
    {/snippet}
</NamedSnippetOnly>

<!-- Has actual content - SHOULD pass children (this should error since component doesn't expect children) -->
<NamedSnippetOnly icon="test" tooltip="World">
    Some text that is NOT a snippet
</NamedSnippetOnly>
