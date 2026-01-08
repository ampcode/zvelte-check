<script lang="ts">
  let count = 0;

  function increment() {
    count += 1;
  }
</script>

<button on:click={increment}>
  Clicked {count} times
</button>

<style>
  button {
    background: blue;
    color: white;
  }
</style>
