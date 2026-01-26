<script lang="ts">
  import ArrowFnTypeCheck from './ArrowFnTypeCheck.svelte';
  
  async function getData(days: string): Promise<number> {
    return days.length;
  }
</script>

<!-- This should report an error: Argument of type '7 | 14 | 30' is not assignable to parameter of type 'string' -->
<ArrowFnTypeCheck fetchData={(days) => getData(days)} />
