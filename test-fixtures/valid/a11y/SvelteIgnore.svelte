<!-- Test fixture for svelte-ignore pragma -->

<!-- svelte-ignore should suppress the immediately following element's diagnostic -->

<!-- svelte-ignore a11y-missing-alt -->
<img src="/photo.jpg">

<!-- svelte-ignore a11y-missing-href -->
<a>Click me</a>

<!-- Multiple codes in one comment -->
<!-- svelte-ignore a11y-missing-alt a11y-no-redundant-roles -->
<img src="/photo.jpg" role="img">

<!-- svelte-ignore a11y-distracting-elements -->
<marquee>Scrolling text</marquee>

<!-- svelte-ignore a11y-autofocus -->
<button autofocus>Focus me</button>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div on:click={handleClick}>Clickable div</div>

<!-- Regular comment should NOT suppress diagnostic -->
<!-- This is just a regular comment -->
<img src="/should-warn.jpg">
