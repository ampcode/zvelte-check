<!-- Test fixtures for media caption a11y rules -->

<!-- a11y-media-has-caption: video without track -->
<!-- Expected: line 5, code: a11y-media-has-caption -->
<video src="/video.mp4"></video>

<!-- a11y-media-has-caption: video with controls but no captions -->
<!-- Expected: line 9, code: a11y-media-has-caption -->
<video src="/video.mp4" controls></video>

<!-- a11y-media-has-caption: video with autoplay but no captions -->
<!-- Expected: line 13, code: a11y-media-has-caption -->
<video src="/video.mp4" autoplay></video>

<!-- a11y-media-has-caption: video with source child but no track -->
<!-- Expected: line 17, code: a11y-media-has-caption -->
<video>
  <source src="/video.mp4" type="video/mp4">
</video>

<!-- a11y-media-has-caption: audio without track (if required) -->
<!-- Expected: line 23, code: a11y-media-has-caption -->
<audio src="/audio.mp3"></audio>

<!-- Valid: video with captions track -->
<video src="/video.mp4">
  <track kind="captions" src="/captions.vtt">
</video>

<!-- Valid: video with subtitles track -->
<video src="/video.mp4">
  <track kind="subtitles" src="/subs.vtt">
</video>

<!-- Valid: video muted (no audio to caption) -->
<video src="/video.mp4" muted></video>

<!-- Valid: audio with captions track -->
<audio src="/audio.mp3">
  <track kind="captions" src="/captions.vtt">
</audio>
