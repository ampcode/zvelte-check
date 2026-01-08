<!--
  Dynamic class directives.
  Tests that class: directives are recognized as used and don't false-positive.
-->
<script lang="ts">
    let isActive = true;
    let isDisabled = false;
    let isPrimary = true;
    let isLarge = false;
    let status = 'success';
</script>

<!-- Classes applied via class: directive should be recognized -->
<div class="container">
    <button class:active={isActive} class:disabled={isDisabled}>
        Toggle Button
    </button>

    <span class:primary={isPrimary} class:large={isLarge}>
        Styled Span
    </span>

    <!-- Shorthand class directive -->
    <div class:isActive class:isDisabled>
        Shorthand syntax
    </div>

    <!-- Multiple classes on same element -->
    <p class="base" class:highlight={true} class:bold={true}>
        Multiple dynamic
    </p>
</div>

<style>
    /* Used via class: directive - should NOT warn */
    .active {
        background: green;
    }

    .disabled {
        opacity: 0.5;
        pointer-events: none;
    }

    .primary {
        color: blue;
    }

    .large {
        font-size: 2rem;
    }

    /* Used via shorthand class:varName - should NOT warn */
    .isActive {
        border: 2px solid green;
    }

    .isDisabled {
        cursor: not-allowed;
    }

    /* Used directly and via class: */
    .base {
        padding: 0.5rem;
    }

    .highlight {
        background: yellow;
    }

    .bold {
        font-weight: bold;
    }

    /* Used container class */
    .container {
        display: flex;
        flex-direction: column;
        gap: 1rem;
    }

    /* Unused class - should warn */
    /* Expected: line 82, code: css-unused-selector, message: Unused CSS selector ".never-used" */
    .never-used {
        display: none;
    }
</style>
