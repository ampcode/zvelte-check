<!--
  CSS comments handling.
  Tests that CSS comments are properly parsed and don't affect unused selector detection.
-->
<script lang="ts">
    let visible = true;
</script>

<div class="card">
    <h2 class="title">Card Title</h2>
    <p class="content">Card content goes here.</p>
</div>

<style>
    /* This is a comment about the card */
    .card {
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 1rem;
    }

    /* Multi-line comment
       describing the title styling
       with multiple lines */
    .title {
        font-size: 1.5rem;
        margin: 0 0 1rem;
    }

    /**
     * JSDoc-style comment
     * @description Content paragraph styling
     */
    .content {
        color: #666;
        line-height: 1.5;
    }

    /* Commented-out selector should NOT trigger warning */
    /* .old-class { color: red; } */

    /*
    .another-old-class {
        display: none;
    }
    */

    /* Unused selector after comments - should still warn */
    /* Expected: line 51, code: css-unused-selector, message: Unused CSS selector ".footer" */
    .footer {
        margin-top: 2rem;
    }

    /* Comment with special characters: @#$%^&*(){}[]|\ */
    /* Expected: line 57, code: css-unused-selector, message: Unused CSS selector ".special" */
    .special {
        content: "test";
    }

    /* Empty comment *//**/
    /* Expected: line 63, code: css-unused-selector, message: Unused CSS selector ".after-empty" */
    .after-empty {
        visibility: hidden;
    }
</style>
