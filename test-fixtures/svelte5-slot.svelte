<!-- 
  Svelte 5 component using runes and <slot>.
  SHOULD produce slot_element_deprecated warning because
  this component uses Svelte 5 syntax and should use {@render children()}.
-->
<script lang="ts">
  interface Props {
    title: string;
  }
  
  let { title }: Props = $props();
</script>

<div class="container">
  <h1>{title}</h1>
  <slot />
</div>
