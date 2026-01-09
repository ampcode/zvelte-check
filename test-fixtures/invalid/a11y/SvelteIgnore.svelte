<!-- Test fixture for svelte-ignore pragma -->

<!-- svelte-ignore should suppress the immediately following element's diagnostic -->

<!-- svelte-ignore a11y_missing_attribute -->
<img src="/photo.jpg">

<!-- svelte-ignore a11y_invalid_attribute -->
<a>Click me</a>

<!-- Multiple codes in one comment -->
<!-- svelte-ignore a11y_missing_attribute a11y_no_redundant_roles -->
<img src="/photo.jpg" role="img">

<!-- svelte-ignore a11y_distracting_elements -->
<marquee>Scrolling text</marquee>

<!-- svelte-ignore a11y_autofocus -->
<button autofocus>Focus me</button>

<!-- svelte-ignore a11y_click_events_have_key_events a11y_no_static_element_interactions -->
<div on:click={handleClick}>Clickable div</div>

<!-- Regular comment should NOT suppress diagnostic -->
<!-- This is just a regular comment -->
<img src="/should-warn.jpg">
