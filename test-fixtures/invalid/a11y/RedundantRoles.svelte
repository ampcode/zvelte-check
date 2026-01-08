<!-- Test fixtures for redundant role a11y rules -->

<!-- a11y-no-redundant-roles: button with role="button" -->
<!-- Expected: line 5, code: a11y-no-redundant-roles -->
<button role="button">Redundant button role</button>

<!-- a11y-no-redundant-roles: anchor with role="link" -->
<!-- Expected: line 9, code: a11y-no-redundant-roles -->
<a href="/link" role="link">Redundant link role</a>

<!-- a11y-no-redundant-roles: img with role="img" -->
<!-- Expected: line 13, code: a11y-no-redundant-roles -->
<img src="/photo.jpg" alt="Photo" role="img">

<!-- a11y-no-redundant-roles: nav with role="navigation" -->
<!-- Expected: line 17, code: a11y-no-redundant-roles -->
<nav role="navigation">Redundant nav role</nav>

<!-- a11y-no-redundant-roles: main with role="main" -->
<!-- Expected: line 21, code: a11y-no-redundant-roles -->
<main role="main">Redundant main role</main>

<!-- a11y-no-redundant-roles: article with role="article" -->
<!-- Expected: line 25, code: a11y-no-redundant-roles -->
<article role="article">Redundant article role</article>

<!-- a11y-no-redundant-roles: aside with role="complementary" -->
<!-- Expected: line 29, code: a11y-no-redundant-roles -->
<aside role="complementary">Redundant aside role</aside>

<!-- a11y-no-redundant-roles: h1 with role="heading" -->
<!-- Expected: line 33, code: a11y-no-redundant-roles -->
<h1 role="heading">Redundant heading role</h1>

<!-- a11y-no-redundant-roles: h2 with role="heading" -->
<!-- Expected: line 37, code: a11y-no-redundant-roles -->
<h2 role="heading">Redundant heading role</h2>

<!-- a11y-no-redundant-roles: ul with role="list" -->
<!-- Expected: line 41, code: a11y-no-redundant-roles -->
<ul role="list">Redundant list role</ul>

<!-- a11y-no-redundant-roles: ol with role="list" -->
<!-- Expected: line 45, code: a11y-no-redundant-roles -->
<ol role="list">Redundant list role</ol>

<!-- a11y-no-redundant-roles: li with role="listitem" -->
<!-- Expected: line 49, code: a11y-no-redundant-roles -->
<ul>
  <li role="listitem">Redundant listitem role</li>
</ul>

<!-- a11y-no-redundant-roles: table with role="table" -->
<!-- Expected: line 55, code: a11y-no-redundant-roles -->
<table role="table">
  <tr><td>Redundant table role</td></tr>
</table>

<!-- a11y-no-redundant-roles: hr with role="separator" -->
<!-- Expected: line 61, code: a11y-no-redundant-roles -->
<hr role="separator">

<!-- a11y-no-redundant-roles: section with role="region" -->
<!-- Expected: line 65, code: a11y-no-redundant-roles -->
<section role="region">Redundant section role</section>

<!-- Valid: different roles -->
<button role="switch">Different role is fine</button>
<a href="/link" role="button">Different role is fine</a>
<div role="button">Div needs role</div>
