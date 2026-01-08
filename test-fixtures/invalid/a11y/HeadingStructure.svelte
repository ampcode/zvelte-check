<!-- Test fixtures for heading structure a11y rules -->

<!-- a11y-heading-order: skipping from h1 to h3 -->
<h1>Main Title</h1>
<!-- Expected: line 6, code: a11y-heading-order -->
<h3>Skipped h2</h3>

<!-- a11y-heading-order: skipping from h1 to h4 -->
<h1>Title</h1>
<!-- Expected: line 11, code: a11y-heading-order -->
<h4>Skipped two levels</h4>

<!-- a11y-heading-order: skipping from h2 to h5 -->
<h1>Page Title</h1>
<h2>Section</h2>
<!-- Expected: line 18, code: a11y-heading-order -->
<h5>Skipped from h2 to h5</h5>

<!-- a11y-heading-order: skipping from h3 to h5 -->
<h1>Article Title</h1>
<h3>First subsection</h3>
<!-- Expected: line 25, code: a11y-heading-order -->
<h5>Deep subsection</h5>
<!-- Expected: line 27, code: a11y-heading-order -->
<h6>Even deeper</h6>

<!-- a11y-heading-order: skipping from h1 to h6 -->
<h1>Top</h1>
<!-- Expected: line 32, code: a11y-heading-order -->
<h6>Skipped to h6</h6>

<!-- Valid: proper heading order -->
<h1>Level 1</h1>
<h2>Level 2</h2>
<h3>Level 3</h3>
<h4>Level 4</h4>
<h5>Level 5</h5>
<h6>Level 6</h6>

<!-- Valid: going back up is fine -->
<h1>Section 1</h1>
<h2>Subsection</h2>
<h1>Section 2</h1>

<!-- Valid: same level repeatedly -->
<h2>Item 1</h2>
<h2>Item 2</h2>
<h2>Item 3</h2>
