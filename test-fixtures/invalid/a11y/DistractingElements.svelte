<!-- Test fixtures for distracting elements a11y rules -->

<!-- a11y-distracting-elements: marquee element -->
<!-- Expected: line 5, code: a11y-distracting-elements -->
<marquee>Scrolling text is distracting</marquee>

<!-- a11y-distracting-elements: marquee with direction -->
<!-- Expected: line 9, code: a11y-distracting-elements -->
<marquee direction="right">Right scrolling</marquee>

<!-- a11y-distracting-elements: marquee with behavior -->
<!-- Expected: line 13, code: a11y-distracting-elements -->
<marquee behavior="alternate">Bouncing text</marquee>

<!-- a11y-distracting-elements: blink element -->
<!-- Expected: line 17, code: a11y-distracting-elements -->
<blink>Blinking text is even worse</blink>

<!-- a11y-distracting-elements: nested marquee -->
<!-- Expected: line 21, code: a11y-distracting-elements -->
<div>
  <marquee>Nested scrolling</marquee>
</div>

<!-- a11y-distracting-elements: marquee in article -->
<!-- Expected: line 27, code: a11y-distracting-elements -->
<article>
  <marquee>Scrolling in article</marquee>
</article>

<!-- a11y-distracting-elements: blink in header -->
<!-- Expected: line 33, code: a11y-distracting-elements -->
<header>
  <blink>Blinking in header</blink>
</header>

<!-- Multiple distracting elements -->
<!-- Expected: line 39, code: a11y-distracting-elements -->
<!-- Expected: line 40, code: a11y-distracting-elements -->
<marquee>First scrolling text</marquee>
<marquee>Second scrolling text</marquee>

<!-- Combined distracting elements -->
<!-- Expected: line 45, code: a11y-distracting-elements -->
<!-- Expected: line 46, code: a11y-distracting-elements -->
<marquee>
  <blink>Scrolling and blinking</blink>
</marquee>
