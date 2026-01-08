<!-- Test fixtures for static element interactions a11y rules -->

<!-- a11y-no-static-element-interactions: div with click needs role -->
<!-- Expected: line 5, code: a11y-no-static-element-interactions -->
<div on:click={() => {}}>Static div needs role</div>

<!-- a11y-no-static-element-interactions: span with click needs role -->
<!-- Expected: line 9, code: a11y-no-static-element-interactions -->
<span on:click={() => {}}>Static span needs role</span>

<!-- a11y-no-static-element-interactions: section with click needs role -->
<!-- Expected: line 13, code: a11y-no-static-element-interactions -->
<section on:click={() => {}}>Static section needs role</section>

<!-- a11y-no-static-element-interactions: article with click needs role -->
<!-- Expected: line 17, code: a11y-no-static-element-interactions -->
<article on:click={() => {}}>Static article needs role</article>

<!-- a11y-no-static-element-interactions: main with click needs role -->
<!-- Expected: line 21, code: a11y-no-static-element-interactions -->
<main on:click={() => {}}>Static main needs role</main>

<!-- a11y-no-static-element-interactions: aside with click needs role -->
<!-- Expected: line 25, code: a11y-no-static-element-interactions -->
<aside on:click={() => {}}>Static aside needs role</aside>

<!-- a11y-no-static-element-interactions: header with click needs role -->
<!-- Expected: line 29, code: a11y-no-static-element-interactions -->
<header on:click={() => {}}>Static header needs role</header>

<!-- a11y-no-static-element-interactions: footer with click needs role -->
<!-- Expected: line 33, code: a11y-no-static-element-interactions -->
<footer on:click={() => {}}>Static footer needs role</footer>

<!-- Valid: div with role="button" -->
<div role="button" on:click={() => {}}>Has button role</div>

<!-- Valid: div with role="link" -->
<div role="link" on:click={() => {}}>Has link role</div>

<!-- Valid: button element (inherently interactive) -->
<button on:click={() => {}}>Button element</button>

<!-- Valid: anchor element (inherently interactive) -->
<a href="/page" on:click={() => {}}>Anchor element</a>

<!-- Valid: div with role="presentation" (explicitly non-interactive) -->
<div role="presentation" on:click={() => {}}>Presentation role</div>
