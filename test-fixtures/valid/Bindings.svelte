<!--
  Valid component demonstrating various bindings.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        externalValue?: string;
    }

    let { externalValue = '' }: Props = $props();

    // Form bindings
    let textValue = $state('');
    let numberValue = $state(0);
    let checked = $state(false);
    let selected = $state('option1');
    let multiSelected: string[] = $state([]);
    let groupValue = $state('a');

    // Element bindings
    let inputElement: HTMLInputElement | null = $state(null);
    let divElement: HTMLDivElement | null = $state(null);

    // Dimension bindings
    let clientWidth = $state(0);
    let clientHeight = $state(0);

    function focusInput() {
        inputElement?.focus();
    }
</script>

<div class="bindings-demo" bind:this={divElement} bind:clientWidth bind:clientHeight>
    <p>Container size: {clientWidth}×{clientHeight}</p>

    <div class="field">
        <label for="text-field">Text input:</label>
        <input
            id="text-field"
            type="text"
            bind:value={textValue}
            bind:this={inputElement}
        />
        <button type="button" onclick={focusInput}>Focus</button>
    </div>

    <div class="field">
        <label for="number-field">Number input:</label>
        <input id="number-field" type="number" bind:value={numberValue} />
    </div>

    <div class="field">
        <label>
            <input type="checkbox" bind:checked />
            Checkbox is {checked ? 'checked' : 'unchecked'}
        </label>
    </div>

    <fieldset>
        <legend>Radio group:</legend>
        <label>
            <input type="radio" bind:group={groupValue} value="a" />
            Option A
        </label>
        <label>
            <input type="radio" bind:group={groupValue} value="b" />
            Option B
        </label>
        <label>
            <input type="radio" bind:group={groupValue} value="c" />
            Option C
        </label>
        <p>Selected: {groupValue}</p>
    </fieldset>

    <div class="field">
        <label for="select-field">Select:</label>
        <select id="select-field" bind:value={selected}>
            <option value="option1">Option 1</option>
            <option value="option2">Option 2</option>
            <option value="option3">Option 3</option>
        </select>
    </div>

    <div class="field">
        <label for="multi-select">Multi-select:</label>
        <select id="multi-select" multiple bind:value={multiSelected}>
            <option value="red">Red</option>
            <option value="green">Green</option>
            <option value="blue">Blue</option>
        </select>
        <p>Selected: {multiSelected.join(', ')}</p>
    </div>
</div>

<style>
    .bindings-demo {
        padding: 1rem;
        border: 1px solid #ccc;
    }

    .field {
        margin-bottom: 1rem;
    }

    label {
        display: block;
        margin-bottom: 0.25rem;
    }

    fieldset {
        margin-bottom: 1rem;
        padding: 0.5rem;
    }

    fieldset label {
        display: inline-flex;
        align-items: center;
        margin-right: 1rem;
    }

    input[type="text"],
    input[type="number"],
    select {
        padding: 0.5rem;
        border: 1px solid #ccc;
        border-radius: 4px;
    }
</style>
