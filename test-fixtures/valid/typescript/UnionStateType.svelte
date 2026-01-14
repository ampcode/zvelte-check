<script lang="ts">
    // Regression test for: union type narrowing false positives
    // When using $state with explicit union type annotation,
    // TypeScript should preserve the full union type, not narrow to the initial value.
    
    let activeTab: 'cli' | 'vscode' = $state('cli');
    let status: 'idle' | 'loading' | 'error' = $state('idle');
    
    // These comparisons should NOT cause "types have no overlap" errors
    const isVscode = activeTab === 'vscode';
    const isError = status === 'error';
</script>

<div class={activeTab === 'vscode' ? 'active' : 'hidden'}>
    {#if status === 'loading'}
        Loading...
    {:else if status === 'error'}
        Error!
    {/if}
</div>
