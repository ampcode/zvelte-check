<!--
  $bindable() rune for two-way binding props.
  Tests parser handling of $bindable in $props destructuring.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        value?: string;
        count?: number;
        checked?: boolean;
    }
    
    // Basic $bindable with default
    let { 
        value = $bindable('default'),
        count = $bindable(0),
        checked = $bindable(false)
    }: Props = $props();
    
    // Using the bindable values
    function increment() {
        count += 1;
    }
    
    function toggle() {
        checked = !checked;
    }
    
    function clear() {
        value = '';
    }
</script>

<div class="bindable-demo">
    <input bind:value type="text" />
    <button onclick={clear}>Clear</button>
    
    <p>Count: {count}</p>
    <button onclick={increment}>Increment</button>
    
    <label>
        <input type="checkbox" bind:checked />
        Checked: {checked}
    </label>
    <button onclick={toggle}>Toggle</button>
</div>

<style>
    .bindable-demo {
        display: flex;
        flex-direction: column;
        gap: 1rem;
    }
</style>
