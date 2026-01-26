<!-- 
  Svelte 4 component using export let and <slot>.
  Should NOT produce slot_element_deprecated warning because
  this component uses Svelte 4 syntax, where <slot> is the correct approach.
  
  Regression test for: zvelte-check was incorrectly warning about <slot>
  in Svelte 4 components. The warning should only apply to Svelte 5
  components that use runes ($props, $state, etc.).
-->
<script>
  export let title = '';
</script>

<div class="container">
  <h1>{title}</h1>
  <slot />
</div>
