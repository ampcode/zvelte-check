<!-- Regression test: variables declared in outer callbacks should be visible in nested callbacks.
     The pattern: onfocus={(e) => { const input = ...; requestAnimationFrame(() => { input.method() }) }}
     Should NOT produce "Cannot find name 'input'" for the nested callback reference. -->
<script lang="ts">
  import type { Component } from 'svelte'

  interface InputProps {
    onfocus?: (e: FocusEvent & { currentTarget: HTMLInputElement }) => void
  }

  let Input: Component<InputProps>
</script>

<!-- Pattern from message-card.svelte:188-196 -->
<Input
  onfocus={(e) => {
    const input = e.currentTarget
    if (!input.value) {
      requestAnimationFrame(() => {
        input.setSelectionRange(8, 8)
      })
    }
  }}
/>
