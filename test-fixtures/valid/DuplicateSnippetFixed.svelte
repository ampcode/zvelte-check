<!--
  Test case: mixed imports with type specifiers from 'svelte'.
  The transformer adds `import type { Snippet } from 'svelte'` automatically.
  This file tests that mixed imports like `import { type Snippet, tick }` have
  the type specifier filtered out to avoid duplicate identifier errors.
  Should produce zero TS diagnostics (svelte module resolution errors are expected
  in test fixtures without node_modules).
-->
<script lang="ts">
    import { type Snippet, tick } from 'svelte';

    interface Props {
        header?: Snippet;
    }

    let { header }: Props = $props();
</script>

{#if header}
    {@render header()}
{/if}
