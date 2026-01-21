<script lang="ts">
    import type { Snippet } from 'svelte';
    
    // Component that only accepts a named snippet prop, NOT children
    interface Props {
        icon: any;
        tooltip: string;
        openTooltipSnippet?: Snippet;
    }
    
    let { icon, tooltip, openTooltipSnippet }: Props = $props();
</script>

<div>
    {tooltip}
    {#if openTooltipSnippet}
        {@render openTooltipSnippet()}
    {/if}
</div>
