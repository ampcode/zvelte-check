<!--
  Valid component with properly typed props, events, and bindings.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        name: string;
        count?: number;
        items?: string[];
        onclick?: (event: MouseEvent) => void;
        onchange?: (value: string) => void;
    }

    let { name, count = 0, items = [], onclick, onchange }: Props = $props();

    let inputValue = $state('');

    function handleInput(event: Event) {
        const target = event.target as HTMLInputElement;
        inputValue = target.value;
        onchange?.(inputValue);
    }
</script>

<div>
    <h2>Hello, {name}!</h2>
    <p>Count: {count}</p>
    
    <label for="text-input">Enter text:</label>
    <input id="text-input" type="text" value={inputValue} oninput={handleInput} />
    
    <button type="button" {onclick}>
        Click me
    </button>
    
    <ul>
        {#each items as item}
            <li>{item}</li>
        {/each}
    </ul>
</div>

<style>
    div {
        padding: 1rem;
    }

    h2 {
        margin-bottom: 0.5rem;
    }

    button {
        cursor: pointer;
    }

    ul {
        list-style: disc;
        padding-left: 1.5rem;
    }
</style>
