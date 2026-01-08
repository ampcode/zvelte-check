<!--
  Valid interactive elements with proper keyboard support.
  Should produce zero diagnostics.
-->
<script lang="ts">
    let isOpen = $state(false);
    let selectedTab = $state(0);

    function toggleMenu() {
        isOpen = !isOpen;
    }

    function handleKeydown(event: KeyboardEvent) {
        if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            toggleMenu();
        }
    }

    function selectTab(index: number) {
        selectedTab = index;
    }

    const tabs = ['Overview', 'Details', 'Reviews'];
</script>

<!-- Button elements are naturally focusable and have keyboard support -->
<button type="button" onclick={toggleMenu} aria-expanded={isOpen}>
    Toggle Menu
</button>

<!-- Link with href is properly accessible -->
<a href="/about">About Us</a>

<!-- Interactive div with proper role, tabindex, and keyboard handler -->
<div
    role="button"
    tabindex="0"
    onclick={toggleMenu}
    onkeydown={handleKeydown}
    aria-pressed={isOpen}
>
    Custom Button
</div>

<!-- Accessible tabs pattern -->
<div class="tabs" role="tablist" aria-label="Content sections">
    {#each tabs as tab, index}
        <button
            type="button"
            role="tab"
            id="tab-{index}"
            aria-selected={selectedTab === index}
            aria-controls="panel-{index}"
            tabindex={selectedTab === index ? 0 : -1}
            onclick={() => selectTab(index)}
        >
            {tab}
        </button>
    {/each}
</div>

{#each tabs as tab, index}
    <div
        role="tabpanel"
        id="panel-{index}"
        aria-labelledby="tab-{index}"
        hidden={selectedTab !== index}
    >
        <p>Content for {tab} tab</p>
    </div>
{/each}

<!-- Accessible dropdown menu -->
{#if isOpen}
    <nav aria-label="Main navigation">
        <ul role="menu">
            <li role="menuitem"><a href="/home">Home</a></li>
            <li role="menuitem"><a href="/products">Products</a></li>
            <li role="menuitem"><a href="/contact">Contact</a></li>
        </ul>
    </nav>
{/if}

<style>
    .tabs {
        display: flex;
        gap: 0.5rem;
        border-bottom: 1px solid #ccc;
    }

    [role="tab"] {
        padding: 0.5rem 1rem;
        border: none;
        background: transparent;
        cursor: pointer;
    }

    [role="tab"][aria-selected="true"] {
        border-bottom: 2px solid #007bff;
    }

    [role="tabpanel"] {
        padding: 1rem;
    }

    [role="menu"] {
        list-style: none;
        padding: 0;
        margin: 0;
    }
</style>
