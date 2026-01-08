<script lang="ts">
    import { Button } from '$lib';
    import type { PageData } from './$types';

    interface Props {
        data: PageData;
    }

    let { data }: Props = $props();
    let count = $state(0);

    function increment() {
        count += 1;
    }
</script>

<svelte:head>
    <title>{data.title}</title>
</svelte:head>

<h1>{data.title}</h1>

<p>Count: {count}</p>
<Button label="Increment" onclick={increment} />

<ul>
    {#each data.items as item}
        <li>{item}</li>
    {/each}
</ul>

<style>
    h1 {
        color: #333;
    }

    ul {
        list-style: none;
        padding: 0;
    }
</style>
