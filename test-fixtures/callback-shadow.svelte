<script lang="ts">
  import type { Component } from 'svelte'
  
  interface PromptEditorProps {
    value: string
    oninput?: (value: string) => void
  }
  
  let PromptEditor: Component<PromptEditorProps>
  
  let thread = $state<{ messages: string[] } | null>(null)
  function setDraft(val: string) { console.log(val) }
</script>

<PromptEditor
  value="test"
  oninput={(value) => {
    if (thread?.messages.length === 0) {
      setDraft(value)
    }
  }}
/>
