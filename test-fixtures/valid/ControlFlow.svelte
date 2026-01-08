<!--
  Valid component demonstrating Svelte control flow blocks.
  Should produce zero diagnostics.
-->
<script lang="ts">
    type LoadingState = 
        | { status: 'idle' }
        | { status: 'loading' }
        | { status: 'success'; data: string[] }
        | { status: 'error'; message: string };

    interface Props {
        items?: string[];
        showAdvanced?: boolean;
    }

    let { items = [], showAdvanced = false }: Props = $props();

    let loadingState: LoadingState = $state({ status: 'idle' });
    let promise: Promise<string> | null = $state(null);

    async function fetchData(): Promise<string> {
        await new Promise(resolve => setTimeout(resolve, 1000));
        if (Math.random() > 0.5) {
            return 'Fetched data!';
        }
        throw new Error('Random failure');
    }

    function startFetch() {
        promise = fetchData();
    }
</script>

<div class="control-flow-demo">
    <!-- #if / :else if / :else -->
    <section>
        <h3>Conditional rendering</h3>
        {#if loadingState.status === 'loading'}
            <p>Loading...</p>
        {:else if loadingState.status === 'success'}
            <ul>
                {#each loadingState.data as item}
                    <li>{item}</li>
                {/each}
            </ul>
        {:else if loadingState.status === 'error'}
            <p class="error">{loadingState.message}</p>
        {:else}
            <p>Ready to load</p>
        {/if}
    </section>

    <!-- #each with index and key -->
    <section>
        <h3>List rendering</h3>
        {#each items as item, index (item)}
            <div class="item">
                <span class="index">{index + 1}.</span>
                {item}
            </div>
        {:else}
            <p>No items available</p>
        {/each}
    </section>

    <!-- #await -->
    <section>
        <h3>Async handling</h3>
        <button type="button" onclick={startFetch}>Fetch Data</button>
        
        {#if promise}
            {#await promise}
                <p>Waiting for response...</p>
            {:then value}
                <p class="success">{value}</p>
            {:catch error}
                <p class="error">{error.message}</p>
            {/await}
        {/if}
    </section>

    <!-- @const for computed values in templates -->
    <section>
        <h3>Computed values</h3>
        {#each items as item, index}
            {@const isFirst = index === 0}
            {@const isLast = index === items.length - 1}
            {@const position = isFirst ? 'first' : isLast ? 'last' : 'middle'}
            <p class={position}>{item} ({position})</p>
        {/each}
    </section>

    <!-- Nested control flow -->
    {#if showAdvanced}
        <section>
            <h3>Advanced features</h3>
            {#each items as category}
                <details>
                    <summary>{category}</summary>
                    {#if category.length > 5}
                        <p>Long category name</p>
                    {:else}
                        <p>Short category name</p>
                    {/if}
                </details>
            {/each}
        </section>
    {/if}
</div>

<style>
    section {
        margin-bottom: 1.5rem;
        padding: 1rem;
        border: 1px solid #eee;
        border-radius: 4px;
    }

    h3 {
        margin-top: 0;
    }

    .item {
        padding: 0.25rem 0;
    }

    .index {
        color: #888;
    }

    .success {
        color: #28a745;
    }

    .error {
        color: #dc3545;
    }

    .first {
        font-weight: bold;
    }

    .last {
        font-style: italic;
    }
</style>
