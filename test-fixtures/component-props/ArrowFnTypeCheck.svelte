<script lang="ts">
  // Component that accepts a callback prop with specific parameter type
  interface Props {
    fetchData: (days: 7 | 14 | 30) => Promise<number>;
  }
  let { fetchData }: Props = $props();
</script>
