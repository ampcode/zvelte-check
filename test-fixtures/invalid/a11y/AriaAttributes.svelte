<!-- Test fixtures for ARIA attribute a11y rules -->

<!-- a11y-unknown-aria: invalid aria attribute -->
<!-- Expected: line 5, code: a11y-unknown-aria -->
<div aria-labeledby="test">Misspelled (should be labelledby)</div>

<!-- a11y-unknown-aria: made-up aria attribute -->
<!-- Expected: line 9, code: a11y-unknown-aria -->
<div aria-foo="bar">Invalid aria attribute</div>

<!-- a11y-unknown-aria: aria-role doesn't exist -->
<!-- Expected: line 13, code: a11y-unknown-aria -->
<div aria-role="button">Should use role, not aria-role</div>

<!-- a11y-unknown-aria: another invalid -->
<!-- Expected: line 17, code: a11y-unknown-aria -->
<span aria-custom="value">Custom aria (invalid)</span>

<!-- Valid: correct aria attributes -->
<div aria-label="Menu">Valid label</div>
<div aria-labelledby="heading1">Valid labelledby</div>
<div aria-describedby="desc">Valid describedby</div>
<div aria-hidden="true">Valid hidden</div>
<div aria-expanded="false">Valid expanded</div>
<button aria-pressed="true">Valid pressed</button>

<!-- a11y-hidden-focusable: aria-hidden on focusable element -->
<!-- Expected: line 29, code: a11y-hidden-focusable -->
<button aria-hidden="true">Hidden focusable button</button>

<!-- a11y-hidden-focusable: aria-hidden on link -->
<!-- Expected: line 33, code: a11y-hidden-focusable -->
<a href="/link" aria-hidden="true">Hidden focusable link</a>

<!-- a11y-hidden-focusable: aria-hidden on element with tabindex -->
<!-- Expected: line 37, code: a11y-hidden-focusable -->
<div tabindex="0" aria-hidden="true">Focusable div hidden</div>

<!-- Valid: aria-hidden="false" is fine -->
<button aria-hidden="false">Not hidden</button>

<!-- Valid: aria-hidden on non-focusable element -->
<div aria-hidden="true">Decorative element</div>
