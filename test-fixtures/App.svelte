<script lang="ts">
  import Button from './Button.svelte';
  
  let name: string = 'world';
  let count: number = 0;
  
  // This would be a type error if TS checking worked
  let badType: number = "not a number";
</script>

<main>
  <h1>Hello {name}!</h1>
  <Button />
  <p>Count: {count}</p>
</main>

<style>
  main {
    padding: 1rem;
  }
  
  /* This class is unused */
  .unused-class {
    color: red;
  }
</style>
