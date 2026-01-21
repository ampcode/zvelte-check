<script lang="ts">
  import type { Snippet } from 'svelte';
  import Container from './Container.svelte';

  let { icon }: { icon: any } = $props();
  let condition = true;
</script>

<!-- Test: @const inside component inside snippet (reproduction of icon-button-or-close.svelte) -->
{#snippet child({ props }: { props: Record<string, any> })}
  <Container>
    {#if condition}
      {@const IconComponent = icon}
      <IconComponent class="stroke-[1.5]" />
    {/if}
  </Container>
{/snippet}

{@render child({ props: {} })}
