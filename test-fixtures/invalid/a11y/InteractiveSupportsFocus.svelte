<!-- Test fixtures for interactive supports focus a11y rules -->

<!-- a11y-interactive-supports-focus: button role needs tabindex -->
<!-- Expected: line 5, code: a11y-interactive-supports-focus -->
<div role="button">Interactive role needs tabindex</div>

<!-- a11y-interactive-supports-focus: link role needs tabindex -->
<!-- Expected: line 9, code: a11y-interactive-supports-focus -->
<span role="link">Link role needs tabindex</span>

<!-- a11y-interactive-supports-focus: checkbox role needs tabindex -->
<!-- Expected: line 13, code: a11y-interactive-supports-focus -->
<div role="checkbox">Checkbox role needs tabindex</div>

<!-- a11y-interactive-supports-focus: menuitem role needs tabindex -->
<!-- Expected: line 17, code: a11y-interactive-supports-focus -->
<div role="menuitem">Menuitem role needs tabindex</div>

<!-- a11y-interactive-supports-focus: tab role needs tabindex -->
<!-- Expected: line 21, code: a11y-interactive-supports-focus -->
<div role="tab">Tab role needs tabindex</div>

<!-- a11y-interactive-supports-focus: switch role needs tabindex -->
<!-- Expected: line 25, code: a11y-interactive-supports-focus -->
<div role="switch">Switch role needs tabindex</div>

<!-- a11y-interactive-supports-focus: slider role needs tabindex -->
<!-- Expected: line 29, code: a11y-interactive-supports-focus -->
<div role="slider">Slider role needs tabindex</div>

<!-- a11y-interactive-supports-focus: textbox role needs tabindex -->
<!-- Expected: line 33, code: a11y-interactive-supports-focus -->
<div role="textbox">Textbox role needs tabindex</div>

<!-- Valid: button role with tabindex -->
<div role="button" tabindex="0">Has tabindex</div>

<!-- Valid: link role with tabindex -->
<span role="link" tabindex="0">Has tabindex</span>

<!-- Valid: checkbox role with tabindex -->
<div role="checkbox" tabindex="0">Has tabindex</div>

<!-- Valid: native button element -->
<button role="button">Native button</button>

<!-- Valid: native link element -->
<a href="/page" role="link">Native link</a>
