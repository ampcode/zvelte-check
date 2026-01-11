<!--
  Store assignment syntax: $store = value calls store.set(value).
  This is valid Svelte reactive store syntax.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import { writable } from 'svelte/store'
    
    const countStore = writable(0)
    const nameStore = writable('')
    
    function increment() {
        $countStore = $countStore + 1
    }
    
    function setName(name: string) {
        $nameStore = name
    }
    
    function reset() {
        $countStore = 0
        $nameStore = ''
    }
</script>

<div>
    <p>Count: {$countStore}</p>
    <p>Name: {$nameStore}</p>
    <button onclick={increment}>Increment</button>
    <button onclick={() => setName('Alice')}>Set Name</button>
    <button onclick={reset}>Reset</button>
</div>
