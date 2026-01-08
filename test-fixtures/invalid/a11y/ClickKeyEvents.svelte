<!-- Test fixtures for click/keyboard event a11y rules -->

<!-- a11y-click-events-have-key-events: click without keyboard -->
<!-- Expected: line 5, code: a11y-click-events-have-key-events -->
<div on:click={() => {}}>Clickable div without keyboard</div>

<!-- a11y-click-events-have-key-events: click on span -->
<!-- Expected: line 9, code: a11y-click-events-have-key-events -->
<span on:click={() => {}}>Clickable span without keyboard</span>

<!-- a11y-click-events-have-key-events: click on section -->
<!-- Expected: line 13, code: a11y-click-events-have-key-events -->
<section on:click={() => {}}>Clickable section without keyboard</section>

<!-- a11y-click-events-have-key-events: click on article -->
<!-- Expected: line 17, code: a11y-click-events-have-key-events -->
<article on:click={() => {}}>Clickable article without keyboard</article>

<!-- a11y-click-events-have-key-events: click on paragraph -->
<!-- Expected: line 21, code: a11y-click-events-have-key-events -->
<p on:click={() => {}}>Clickable paragraph without keyboard</p>

<!-- Valid: button with click (buttons are keyboard accessible) -->
<button on:click={() => {}}>Button is fine</button>

<!-- Valid: link with click -->
<a href="/page" on:click={() => {}}>Link is fine</a>

<!-- Valid: div with both click and keydown -->
<div on:click={() => {}} on:keydown={() => {}}>Has keyboard handler</div>

<!-- Valid: div with click and keypress -->
<div on:click={() => {}} on:keypress={() => {}}>Has keypress handler</div>

<!-- Valid: div with click and keyup -->
<div on:click={() => {}} on:keyup={() => {}}>Has keyup handler</div>

<!-- Valid: input with click (form elements are accessible) -->
<input type="checkbox" on:click={() => {}}>
