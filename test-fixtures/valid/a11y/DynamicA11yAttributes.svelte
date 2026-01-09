<!--
  Dynamic a11y attributes.
  When attributes are dynamic expressions, we skip certain a11y checks.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        imageAlt?: string;
        linkHref?: string;
        buttonLabel?: string;
    }
    
    let { imageAlt, linkHref, buttonLabel }: Props = $props();
    
    let dynamicAlt = $state('');
    let condition = $state(true);
</script>

<!-- Dynamic alt attribute - can't check at compile time -->
<img src="/photo.jpg" alt={dynamicAlt}>
<img src="/photo.jpg" alt={imageAlt}>
<img src="/photo.jpg" alt={condition ? 'Visible' : 'Hidden'}>

<!-- Template with dynamic part -->
<img src="/user.jpg" alt="Profile picture of {dynamicAlt}">

<!-- Dynamic href -->
<a href={linkHref}>Dynamic link</a>
<a href={condition ? '/page1' : '/page2'}>Conditional link</a>

<!-- Dynamic aria-label -->
<button aria-label={buttonLabel}>
    <span class="icon">★</span>
</button>

<!-- Dynamic role - we can't determine at compile time if interactive -->
<!-- svelte-ignore a11y_no_noninteractive_tabindex -->
<div role={condition ? 'button' : 'link'} tabindex="0">
    Dynamic role
</div>

<!-- Spread props that might contain a11y attributes -->
<script>
    let imgProps = { alt: 'From spread', src: '/img.jpg' };
</script>
<!-- Note: spread is harder to analyze, so we should be permissive -->

<style>
    .icon {
        font-size: 1.5rem;
    }
</style>
