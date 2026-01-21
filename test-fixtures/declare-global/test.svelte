<script lang="ts">
    import { onMount } from 'svelte';
    
    declare global {
        interface Window {
            __test__: string;
        }
    }
    
    let x = $state(1);
    
    onMount(() => {
        console.log(window.__test__);
    });
</script>

<p>{x}</p>
