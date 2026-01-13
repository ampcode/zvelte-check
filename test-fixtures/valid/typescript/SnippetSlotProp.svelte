<!--
  Test that snippets used as slot props don't cause "declared but never used" warnings.
  Pattern: {#snippet child({ props })} where child is passed to a parent component.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';
    
    interface Props {
        header: Snippet;
        children: Snippet;
    }
    
    let { header, children }: Props = $props();
</script>

<div>
    <!-- Snippet to be passed as a slot prop -->
    {#snippet child({ props }: { props: Record<string, unknown> })}
        <button {...props}>
            Click me
        </button>
    {/snippet}
    
    <!-- Use the snippet -->
    {@render child({ props: { disabled: false } })}
    
    <!-- Render prop snippets -->
    {@render header()}
    {@render children()}
</div>
