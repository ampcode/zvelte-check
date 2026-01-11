<!--
  Snippet parameters shadowing $bindable props.
  This is valid Svelte - snippet params create a new scope.
  Should produce zero diagnostics for redeclaration errors.
-->
<script lang="ts">
    interface Props {
        checked?: boolean;
        indeterminate?: boolean;
    }
    
    let {
        checked = $bindable(false),
        indeterminate = $bindable(false),
    }: Props = $props()
</script>

<div>
    {#snippet children({ checked, indeterminate }: { checked: boolean, indeterminate: boolean })}
        <span>{checked} {indeterminate}</span>
    {/snippet}
</div>
