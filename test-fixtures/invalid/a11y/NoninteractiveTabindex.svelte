<!-- Test fixtures for non-interactive tabindex a11y rules -->

<!-- a11y-no-noninteractive-tabindex: div with tabindex -->
<!-- Expected: line 5, code: a11y-no-noninteractive-tabindex -->
<div tabindex="0">Non-interactive with tabindex</div>

<!-- a11y-no-noninteractive-tabindex: span with tabindex -->
<!-- Expected: line 9, code: a11y-no-noninteractive-tabindex -->
<span tabindex="0">Span with tabindex</span>

<!-- a11y-no-noninteractive-tabindex: paragraph with tabindex -->
<!-- Expected: line 13, code: a11y-no-noninteractive-tabindex -->
<p tabindex="0">Paragraph with tabindex</p>

<!-- a11y-no-noninteractive-tabindex: section with tabindex -->
<!-- Expected: line 17, code: a11y-no-noninteractive-tabindex -->
<section tabindex="0">Section with tabindex</section>

<!-- a11y-no-noninteractive-tabindex: article with tabindex -->
<!-- Expected: line 21, code: a11y-no-noninteractive-tabindex -->
<article tabindex="0">Article with tabindex</article>

<!-- a11y-no-noninteractive-tabindex: heading with tabindex -->
<!-- Expected: line 25, code: a11y-no-noninteractive-tabindex -->
<h1 tabindex="0">Heading with tabindex</h1>

<!-- a11y-no-noninteractive-tabindex: list item with tabindex -->
<!-- Expected: line 29, code: a11y-no-noninteractive-tabindex -->
<ul>
  <li tabindex="0">List item with tabindex</li>
</ul>

<!-- a11y-no-noninteractive-tabindex: blockquote with tabindex -->
<!-- Expected: line 35, code: a11y-no-noninteractive-tabindex -->
<blockquote tabindex="0">Blockquote with tabindex</blockquote>

<!-- a11y-no-noninteractive-tabindex: pre with tabindex -->
<!-- Expected: line 39, code: a11y-no-noninteractive-tabindex -->
<pre tabindex="0">Pre with tabindex</pre>

<!-- Valid: button with tabindex -->
<button tabindex="0">Button is interactive</button>

<!-- Valid: link with tabindex -->
<a href="/page" tabindex="0">Link is interactive</a>

<!-- Valid: input with tabindex -->
<input type="text" tabindex="0">

<!-- Valid: div with tabindex -1 (programmatic focus only) -->
<div tabindex="-1">Programmatic focus only</div>

<!-- Valid: div with role making it interactive -->
<div role="button" tabindex="0">Role makes it interactive</div>
