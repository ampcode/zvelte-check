<script lang="ts">
  type Card = { brand: string; last4: string }
  let card: Card | null = $state(null)
</script>

{#if card}
  {@const { brand, last4 } = card}
  <p>{brand} •••• {last4}</p>
{/if}
