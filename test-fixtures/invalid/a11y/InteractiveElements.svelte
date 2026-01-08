<!-- Test fixtures for interactive element a11y rules -->

<!-- a11y-positive-tabindex: positive tabindex on button -->
<!-- Expected: line 5, code: a11y-positive-tabindex -->
<button tabindex="1">Positive tabindex</button>

<!-- a11y-positive-tabindex: positive tabindex on input -->
<!-- Expected: line 9, code: a11y-positive-tabindex -->
<input type="text" tabindex="2">

<!-- a11y-positive-tabindex: positive tabindex on link -->
<!-- Expected: line 13, code: a11y-positive-tabindex -->
<a href="/link" tabindex="5">Positive tabindex link</a>

<!-- a11y-positive-tabindex: high tabindex value -->
<!-- Expected: line 17, code: a11y-positive-tabindex -->
<div tabindex="999">Very high tabindex</div>

<!-- a11y-positive-tabindex: positive tabindex on div -->
<!-- Expected: line 21, code: a11y-positive-tabindex -->
<div tabindex="3">Div with positive tabindex</div>

<!-- Valid: tabindex="0" is fine -->
<div tabindex="0">Zero tabindex</div>

<!-- Valid: tabindex="-1" is fine -->
<button tabindex="-1">Negative tabindex</button>

<!-- Valid: no tabindex -->
<button>No tabindex</button>

<!-- a11y-autofocus: autofocus attribute -->
<!-- Expected: line 34, code: a11y-autofocus -->
<input type="text" autofocus>

<!-- a11y-autofocus: autofocus on button -->
<!-- Expected: line 38, code: a11y-autofocus -->
<button autofocus>Autofocus button</button>

<!-- a11y-accesskey: accesskey attribute -->
<!-- Expected: line 42, code: a11y-accesskey -->
<button accesskey="s">Save (Alt+S)</button>

<!-- a11y-accesskey: accesskey on link -->
<!-- Expected: line 46, code: a11y-accesskey -->
<a href="/home" accesskey="h">Home (Alt+H)</a>

<!-- a11y-accesskey: accesskey on div -->
<!-- Expected: line 50, code: a11y-accesskey -->
<div accesskey="d">Div with accesskey</div>
