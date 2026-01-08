<!--
  Unused CSS selectors.
  Tests detection of CSS selectors that don't match any elements in the template.
-->
<script lang="ts">
    let active = false;
</script>

<div class="container">
    <p class="text">Hello world</p>
    <button class="btn primary">Click me</button>
</div>

<style>
    /* Used selectors - should not warn */
    .container {
        padding: 1rem;
    }

    .text {
        color: black;
    }

    .btn {
        cursor: pointer;
    }

    .primary {
        background: blue;
    }

    /* Unused class selector */
    /* Expected: line 35, code: css-unused-selector, message: Unused CSS selector ".unused-class" */
    .unused-class {
        color: red;
    }

    /* Unused ID selector */
    /* Expected: line 41, code: css-unused-selector, message: Unused CSS selector "#unused-id" */
    #unused-id {
        margin: 0;
    }

    /* Unused element selector */
    /* Expected: line 47, code: css-unused-selector, message: Unused CSS selector "span" */
    span {
        font-weight: bold;
    }

    /* Unused compound selector */
    /* Expected: line 53, code: css-unused-selector, message: Unused CSS selector "div.missing" */
    div.missing {
        display: none;
    }

    /* Unused descendant selector */
    /* Expected: line 59, code: css-unused-selector, message: Unused CSS selector ".container .missing" */
    .container .missing {
        opacity: 0;
    }

    /* Unused attribute selector */
    /* Expected: line 65, code: css-unused-selector, message: Unused CSS selector "[data-unused]" */
    [data-unused] {
        visibility: hidden;
    }

    /* Unused pseudo-class with nonexistent base */
    /* Expected: line 71, code: css-unused-selector, message: Unused CSS selector ".nonexistent:hover" */
    .nonexistent:hover {
        color: green;
    }
</style>
