<script>
  export let title = '';
  export let description = '';
</script>

<div class="card">
  <h2>{title}</h2>
  <p>{description}</p>
  <slot />
</div>

<style>
  .card {
    border: 1px solid #ccc;
    padding: 1rem;
    border-radius: 8px;
  }
</style>
