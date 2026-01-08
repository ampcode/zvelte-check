<!--
  Valid images with proper alt text and a11y attributes.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        userAvatar?: string;
        userName?: string;
    }

    let { userAvatar = '/default-avatar.png', userName = 'User' }: Props = $props();
</script>

<!-- Informative image with descriptive alt -->
<img src="/hero-image.jpg" alt="A serene mountain landscape at sunset" />

<!-- Decorative image with empty alt -->
<img src="/decorative-border.png" alt="" aria-hidden="true" />

<!-- Image with dynamic alt text -->
<img src={userAvatar} alt="Profile picture of {userName}" />

<!-- Image in a figure with caption -->
<figure>
    <img src="/chart.png" alt="Bar chart showing quarterly sales growth from Q1 to Q4" />
    <figcaption>Quarterly sales performance for 2025</figcaption>
</figure>

<!-- Image as link with proper accessible name -->
<a href="/home" aria-label="Go to homepage">
    <img src="/logo.png" alt="" />
</a>

<!-- Image button with accessible label -->
<button type="button" aria-label="Close dialog">
    <img src="/icons/close.svg" alt="" />
</button>

<!-- SVG with title for accessibility -->
<svg role="img" aria-labelledby="svg-title" viewBox="0 0 24 24">
    <title id="svg-title">Settings</title>
    <path d="M12 15.5A3.5 3.5 0 0 1 8.5 12 3.5 3.5 0 0 1 12 8.5a3.5 3.5 0 0 1 3.5 3.5 3.5 3.5 0 0 1-3.5 3.5" />
</svg>

<style>
    img {
        max-width: 100%;
        height: auto;
    }

    figure {
        margin: 1rem 0;
    }

    figcaption {
        font-size: 0.875rem;
        color: #666;
    }

    button {
        padding: 0.5rem;
        background: transparent;
        border: none;
        cursor: pointer;
    }

    svg {
        width: 24px;
        height: 24px;
    }
</style>
