<!-- Regression test: local variables inside callback closures should not cause false positives.
     Previously, extractMemberAccessChainsFromBody would include method call arguments
     in the extracted chain, causing "Cannot find name" errors for closure-local variables. -->
<script lang="ts">
  import type { Component } from 'svelte'
  import type { Readable } from 'svelte/store'

  interface SettingsToggleProps {
    title: string
    description: string
    checked: boolean
    onclick?: () => void
  }

  let SettingsToggle: Component<SettingsToggleProps>
  let config: Readable<{ settings: Record<string, boolean> } | null>
  $: $config

  interface ViewAPI {
    updateSettings(key: string, value: boolean, scope: string): { subscribe: (cb: object) => void }
  }
  let viewAPI: ViewAPI
</script>

<!-- The onclick callback declares 'current' locally and references it in the method call.
     This should NOT produce a "Cannot find name 'current'" error. -->
<SettingsToggle
  title="Notification Sounds"
  description="Plays a sound when Amp is done or blocked."
  checked={$config?.settings['notifications.enabled'] ?? false}
  onclick={() => {
    const current = $config?.settings['notifications.enabled'] ?? false
    viewAPI.updateSettings('notifications.enabled', !current, 'global').subscribe({})
  }}
/>
