<!-- Test fixtures for missing attribute a11y rules -->

<!-- a11y-missing-alt: img without alt attribute -->
<!-- Expected: line 5, code: a11y-missing-alt -->
<img src="/photo.jpg">

<!-- a11y-missing-alt: img with empty src but still needs alt -->
<!-- Expected: line 9, code: a11y-missing-alt -->
<img src="">

<!-- a11y-missing-alt: img with only src (valid case) -->
<!-- Expected: line 13, code: a11y-missing-alt -->
<img src="/logo.png">

<!-- Valid: img with alt attribute -->
<img src="/photo.jpg" alt="A sunset over the ocean">

<!-- Valid: img with empty alt (decorative image) -->
<img src="/decorative.png" alt="">

<!-- Valid: img with role="presentation" (decorative) -->
<img src="/bg.png" role="presentation">

<!-- Valid: img with role="none" (decorative) -->
<img src="/divider.png" role="none">

<!-- a11y-missing-href: anchor without href -->
<!-- Expected: line 30, code: a11y-missing-href -->
<a>Click me</a>

<!-- a11y-invalid-href: anchor with empty href -->
<!-- Expected: line 34, code: a11y-invalid-href -->
<a href="">Empty link</a>

<!-- a11y-invalid-href: anchor with javascript: href -->
<!-- Expected: line 38, code: a11y-invalid-href -->
<a href="javascript:void(0)">Bad link</a>

<!-- a11y-invalid-href: javascript: with code -->
<!-- Expected: line 42, code: a11y-invalid-href -->
<a href="javascript:alert('hi')">Alert link</a>

<!-- Valid: anchor with proper href -->
<a href="/page">Good link</a>

<!-- Valid: anchor with hash href -->
<a href="#section">Jump link</a>
