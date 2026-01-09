<!--
  Svelte 5 bind:value={get, set} syntax.
  Tests parser handling of getter/setter bindings.
  Should produce zero diagnostics.
-->
<script lang="ts">
    let internalValue = $state('');
    
    // Custom getter and setter functions
    function getValue() {
        return internalValue;
    }
    
    function setValue(newValue: string) {
        internalValue = newValue.toUpperCase();
    }
    
    // For readonly bindings
    let width = $state(0);
    function setWidth(w: number) {
        width = w;
    }
</script>

<!-- Getter/setter syntax for two-way binding -->
<input bind:value={getValue, setValue} />

<!-- Arrow function syntax -->
<input bind:value={() => internalValue, (v) => internalValue = v} />

<!-- Readonly binding with null getter -->
<div bind:clientWidth={null, setWidth}>
    Resize me to update width: {width}px
</div>

<!-- Multiple bindings -->
<input 
    bind:value={getValue, setValue}
    type="text"
    placeholder="Type here..."
/>

<p>Current value: {internalValue}</p>
