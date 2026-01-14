<script lang="ts">
  import ComponentWithExports from './ComponentWithExports.svelte';

  // Using bind:this to get a component ref and call its exported methods.
  // The ref type uses the component name, which should resolve to the instance type
  // (with exported methods) rather than the function type.
  let componentRef = $state<ComponentWithExports | null>(null);

  function handleClick() {
    // These should NOT cause "Property X does not exist on type 'Component<...>'" errors
    componentRef?.focus();
    componentRef?.scrollToBottom();
    componentRef?.focusNext();
  }
</script>

<ComponentWithExports bind:this={componentRef} />
<button onclick={handleClick}>Call methods</button>
