<!-- Test unused-export-let diagnostic -->
<script>
  // Expected: line 4, code: unused-export-let, message: 'unusedProp' is exported but never used
  export let unusedProp;
  
  // This prop is used in template, should not warn
  export let usedProp;
  
  // This prop is used in reactive statement, should not warn  
  export let reactiveInput;
  $: doubled = reactiveInput * 2;
  
  // Expected: line 14, code: unused-export-let, message: 'anotherUnused' is exported but never used
  export let anotherUnused;
</script>

<p>{usedProp}</p>
<p>{doubled}</p>
