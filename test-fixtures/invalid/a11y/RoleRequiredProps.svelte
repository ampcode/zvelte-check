<!-- Test fixtures for role required properties a11y rules -->

<!-- a11y-role-has-required-aria-props: checkbox missing aria-checked -->
<!-- Expected: line 5, code: a11y-role-has-required-aria-props -->
<div role="checkbox">Missing aria-checked</div>

<!-- a11y-role-has-required-aria-props: switch missing aria-checked -->
<!-- Expected: line 9, code: a11y-role-has-required-aria-props -->
<div role="switch">Missing aria-checked</div>

<!-- a11y-role-has-required-aria-props: radio missing aria-checked -->
<!-- Expected: line 13, code: a11y-role-has-required-aria-props -->
<div role="radio">Missing aria-checked</div>

<!-- a11y-role-has-required-aria-props: slider missing aria-valuenow -->
<!-- Expected: line 17, code: a11y-role-has-required-aria-props -->
<div role="slider">Missing aria-valuenow</div>

<!-- a11y-role-has-required-aria-props: spinbutton missing aria-valuenow -->
<!-- Expected: line 21, code: a11y-role-has-required-aria-props -->
<div role="spinbutton">Missing aria-valuenow</div>

<!-- a11y-role-has-required-aria-props: scrollbar missing aria-valuenow -->
<!-- Expected: line 25, code: a11y-role-has-required-aria-props -->
<div role="scrollbar">Missing aria-valuenow</div>

<!-- a11y-role-has-required-aria-props: combobox missing aria-controls and aria-expanded -->
<!-- Expected: line 29, code: a11y-role-has-required-aria-props -->
<div role="combobox">Missing aria-controls and aria-expanded</div>

<!-- a11y-role-has-required-aria-props: option missing aria-selected -->
<!-- Expected: line 33, code: a11y-role-has-required-aria-props -->
<div role="option">Missing aria-selected</div>

<!-- a11y-role-has-required-aria-props: heading missing aria-level -->
<!-- Expected: line 37, code: a11y-role-has-required-aria-props -->
<div role="heading">Missing aria-level</div>

<!-- Valid: separator has no required props (aria-valuenow is optional) -->
<div role="separator">Simple separator</div>
<div role="separator" tabindex="0">Focusable separator</div>
<div role="separator" tabindex="0" aria-valuenow="50">Focusable separator with optional value</div>

<!-- Valid: checkbox with aria-checked -->
<div role="checkbox" aria-checked="false">Has required prop</div>

<!-- Valid: switch with aria-checked -->
<div role="switch" aria-checked="true">Has required prop</div>

<!-- Valid: slider with aria-valuenow -->
<div role="slider" aria-valuenow="50">Has required prop</div>

<!-- Valid: combobox with required props -->
<div role="combobox" aria-controls="list" aria-expanded="false">Has required props</div>

<!-- Valid: heading with aria-level -->
<div role="heading" aria-level="2">Has required prop</div>

<!-- Valid: option with aria-selected -->
<div role="option" aria-selected="false">Has required prop</div>

<!-- Valid: native elements don't need aria props -->
<input type="checkbox">
<input type="radio">
