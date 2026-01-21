<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Props {
        label: string;
        children: Snippet;
    }

    let { label, children }: Props = $props();
</script>

<div class="wrapper">
    <h2>{label}</h2>
    {@render children()}
</div>
