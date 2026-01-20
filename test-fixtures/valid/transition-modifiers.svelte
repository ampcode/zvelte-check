<script lang="ts">
  import { fly, fade } from 'svelte/transition';
  let show = $state(true);
</script>

<!-- Test transition directive modifiers (|global, |local) -->
<div in:fly|global={{ y: 50 }}>in:fly|global should be valid</div>
<div out:fade|local={{ duration: 300 }}>out:fade|local should be valid</div>
<div transition:fly|global={{ x: 100 }}>transition:fly|global should be valid</div>
