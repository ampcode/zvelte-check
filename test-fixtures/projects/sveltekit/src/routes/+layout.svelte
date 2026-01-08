<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Props {
        children: Snippet;
    }

    let { children }: Props = $props();
</script>

<header>
    <nav>
        <a href="/">Home</a>
        <a href="/about">About</a>
    </nav>
</header>

<main>
    {@render children()}
</main>

<style>
    header {
        padding: 1rem;
        background: #f0f0f0;
    }

    nav {
        display: flex;
        gap: 1rem;
    }

    main {
        padding: 1rem;
    }
</style>
