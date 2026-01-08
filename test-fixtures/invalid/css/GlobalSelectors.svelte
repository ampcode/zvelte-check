<!--
  Global CSS selectors.
  Tests that :global() selectors do not trigger unused selector warnings.
-->
<script lang="ts">
    let show = true;
</script>

<div class="wrapper">
    <p>Content</p>
</div>

<style>
    /* Local selector - used */
    .wrapper {
        padding: 1rem;
    }

    /* Global selectors - should NOT warn even though elements don't exist locally */
    :global(body) {
        margin: 0;
    }

    :global(.external-class) {
        color: blue;
    }

    :global(#app) {
        min-height: 100vh;
    }

    /* Nested global inside local - should NOT warn for global part */
    .wrapper :global(.injected) {
        font-size: 14px;
    }

    /* Global with descendant - should NOT warn */
    :global(.parent .child) {
        display: flex;
    }

    /* Global with pseudo-class - should NOT warn */
    :global(html.dark-mode) {
        background: #000;
    }

    /* Global block syntax - should NOT warn */
    :global {
        * {
            box-sizing: border-box;
        }

        html, body {
            height: 100%;
        }
    }

    /* Unused local selector (should still warn) */
    /* Expected: line 59, code: css-unused-selector, message: Unused CSS selector ".local-unused" */
    .local-unused {
        color: red;
    }
</style>
