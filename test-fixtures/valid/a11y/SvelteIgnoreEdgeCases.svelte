<!--
  Edge cases for svelte-ignore pragma handling.
  Tests various ignore syntax patterns.
  Should produce zero diagnostics (except intentional last img warning).
-->

<!-- svelte-ignore with underscore format (Svelte 5 style) -->
<!-- svelte-ignore a11y_missing_attribute -->
<img src="/test.jpg">

<!-- Multiple codes with underscore format -->
<!-- svelte-ignore a11y_autofocus a11y_positive_tabindex -->
<input autofocus tabindex="5" type="text">

<!-- svelte-ignore on same line as element (if supported) -->
<!-- svelte-ignore a11y_distracting_elements --><marquee>Text</marquee>

<!-- svelte-ignore with extra whitespace -->
<!--   svelte-ignore   a11y_missing_attribute   -->
<img src="/test3.jpg">

<!-- svelte-ignore inside control flow -->
{#if true}
    <!-- svelte-ignore a11y_missing_attribute -->
    <img src="/inside-if.jpg">
{/if}

{#each [1, 2] as item}
    <!-- svelte-ignore a11y_missing_attribute -->
    <img src="/item-{item}.jpg">
{/each}

<!-- Deeply nested with svelte-ignore -->
<div>
    <section>
        <!-- svelte-ignore a11y_no_static_element_interactions a11y_click_events_have_key_events -->
        <div on:click={() => {}}>Nested clickable div</div>
    </section>
</div>
