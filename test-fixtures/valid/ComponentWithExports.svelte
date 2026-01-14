<script lang="ts">
  // Component that exports functions for use via bind:this
  export function focus() {
    console.log('focus called');
  }

  export function scrollToBottom() {
    console.log('scrollToBottom called');
  }

  export function focusNext() {
    console.log('focusNext called');
  }
</script>

<div>Component with exports</div>
