<!--
  Strict null checks errors.
  Tests TypeScript strict null checking with Svelte patterns.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Props {
        data?: { value: string };
        items?: string[];
        callback?: () => void;
    }

    let { data, items, callback }: Props = $props();

    // Accessing property on possibly undefined
    // Expected: line 17, code: TS2532, message: Object is possibly 'undefined'
    const value = data.value;

    // Array method on possibly undefined
    // Expected: line 21, code: TS2532, message: Object is possibly 'undefined'
    const count = items.length;

    // Calling possibly undefined function
    // Expected: line 25, code: TS2722, message: Cannot invoke an object which is possibly 'undefined'
    callback();

    // Indexing into possibly undefined array
    // Expected: line 29, code: TS2532, message: Object is possibly 'undefined'
    const first = items[0];

    // Chained access on possibly undefined
    // Expected: line 33, code: TS2532, message: Object is possibly 'undefined'
    const upperValue = data.value.toUpperCase();

    // Destructuring possibly undefined
    // Expected: line 37, code: TS2339, message: Property 'id' does not exist on type 'undefined'
    const { value: v } = data;

    // State that might be null
    let user: { name: string } | null = $state(null);

    // Expected: line 43, code: TS2531, message: Object is possibly 'null'
    const userName = user.name;

    // Async fetch result might be undefined
    async function fetchData(): Promise<void> {
        const response = await fetch('/api/data');
        const json: { result?: string } = await response.json();
        
        // Expected: line 51, code: TS2532, message: Object is possibly 'undefined'
        console.log(json.result.length);
    }
</script>

<!-- Template expressions with null checks -->
<!-- Expected: (error in expression accessing undefined) -->
<p>Value: {data.value}</p>

<!-- Expected: (error accessing length of undefined) -->
<p>Count: {items.length}</p>

{#if data}
    <!-- This is safe due to the conditional -->
    <p>Safe: {data.value}</p>
{/if}
