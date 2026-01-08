<!-- Test that used props don't trigger unused-export-let -->
<script>
  // All these props are used
  export let textContent;
  export let className;
  export let onClick;
  export let show;
  export let items;
  export let bindValue;
</script>

<!-- Direct interpolation -->
<p>{textContent}</p>

<!-- In attribute expression -->
<div class={className}>Content</div>

<!-- In event handler -->
<button on:click={onClick}>Click me</button>

<!-- In if block condition -->
{#if show}
  <span>Visible</span>
{/if}

<!-- In each block -->
{#each items as item}
  <li>{item}</li>
{/each}

<!-- In bind directive -->
<input aria-label="Bind value" bind:value={bindValue}>
