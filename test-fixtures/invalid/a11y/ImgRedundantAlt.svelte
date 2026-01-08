<!-- Test fixtures for redundant alt text a11y rules -->

<!-- a11y-img-redundant-alt: alt starts with "image of" -->
<!-- Expected: line 5, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="Image of a sunset">

<!-- a11y-img-redundant-alt: alt starts with "picture of" -->
<!-- Expected: line 9, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="Picture of mountains">

<!-- a11y-img-redundant-alt: alt starts with "photo of" -->
<!-- Expected: line 13, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="Photo of the team">

<!-- a11y-img-redundant-alt: alt contains "image of" -->
<!-- Expected: line 17, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="This is an image of a cat">

<!-- a11y-img-redundant-alt: alt contains "a picture" -->
<!-- Expected: line 21, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="A picture showing the results">

<!-- a11y-img-redundant-alt: alt contains "photo:" -->
<!-- Expected: line 25, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="Photo: Company logo">

<!-- a11y-img-redundant-alt: uppercase IMAGE -->
<!-- Expected: line 29, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="IMAGE of something">

<!-- a11y-img-redundant-alt: uppercase PICTURE -->
<!-- Expected: line 33, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="PICTURE here">

<!-- a11y-img-redundant-alt: uppercase PHOTO -->
<!-- Expected: line 37, code: a11y-img-redundant-alt -->
<img src="/photo.jpg" alt="PHOTO of event">

<!-- Valid: descriptive alt without redundant words -->
<img src="/photo.jpg" alt="Sunset over the ocean">

<!-- Valid: alt describes content without "image/picture/photo" -->
<img src="/photo.jpg" alt="Team members at conference">

<!-- Valid: empty alt for decorative images -->
<img src="/decorative.png" alt="">

<!-- Valid: alt with similar but not redundant words -->
<img src="/photo.jpg" alt="Photography exhibition">
<img src="/photo.jpg" alt="Imagery from the event">
