<!--
  Test that snippets passed as slots (like Dialog.Trigger child) don't cause
  "declared but never read" warnings. The snippet is used by the parent component.
-->
<script lang="ts">
    interface Props {
        name: string;
    }
    let { name }: Props = $props();
</script>

<div>
    <!-- Snippet that will be passed to a component as a slot -->
    {#snippet child({ props }: { props: Record<string, unknown> })}
        <button {...props}>
            Click {name}
        </button>
    {/snippet}
    
    <!-- Snippet is used via @render -->
    {@render child({ props: { disabled: false } })}
</div>
