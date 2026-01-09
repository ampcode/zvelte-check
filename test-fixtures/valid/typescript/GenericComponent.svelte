<!--
  Generic component using Svelte 5 runes.
  Tests script generics attribute with $props.
  Should produce zero diagnostics.
-->
<script lang="ts" generics="T">
    interface Props {
        value: T;
        defaultValue?: T;
        onChange?: (newValue: T) => void;
    }
    
    let { value, defaultValue, onChange }: Props = $props();
    
    let internalValue = $state(value ?? defaultValue);
    
    function handleChange(newValue: T) {
        internalValue = newValue;
        onChange?.(newValue);
    }
</script>

<div class="generic-component">
    <p>Current value: {JSON.stringify(internalValue)}</p>
    <slot {value} {handleChange} />
</div>

<style>
    .generic-component {
        padding: 1rem;
        border: 1px solid #ddd;
        border-radius: 4px;
    }
</style>
