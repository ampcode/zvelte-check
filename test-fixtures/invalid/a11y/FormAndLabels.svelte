<!-- Test fixtures for form and label a11y rules -->

<!-- a11y-missing-label: input without any label mechanism -->
<!-- Expected: line 5, code: a11y-missing-label -->
<input type="text">

<!-- a11y-missing-label: email input without label -->
<!-- Expected: line 9, code: a11y-missing-label -->
<input type="email">

<!-- a11y-missing-label: password input without label -->
<!-- Expected: line 13, code: a11y-missing-label -->
<input type="password">

<!-- a11y-missing-label: number input without label -->
<!-- Expected: line 17, code: a11y-missing-label -->
<input type="number">

<!-- a11y-missing-label: tel input without label -->
<!-- Expected: line 21, code: a11y-missing-label -->
<input type="tel">

<!-- a11y-missing-label: search input without label -->
<!-- Expected: line 25, code: a11y-missing-label -->
<input type="search">

<!-- a11y-missing-label: textarea without label -->
<!-- Expected: line 29, code: a11y-missing-label -->
<textarea></textarea>

<!-- a11y-missing-label: select without label -->
<!-- Expected: line 33, code: a11y-missing-label -->
<select>
  <option>Option 1</option>
</select>

<!-- Valid: hidden input doesn't need label -->
<input type="hidden" value="secret">

<!-- Valid: submit button doesn't need label -->
<input type="submit" value="Submit">

<!-- Valid: button input doesn't need label -->
<input type="button" value="Click">

<!-- Valid: input with id (could have associated label) -->
<input type="text" id="username">

<!-- Valid: input with aria-label -->
<input type="text" aria-label="Search">

<!-- Valid: input with aria-labelledby -->
<input type="text" aria-labelledby="label1">

<!-- Valid: input with title -->
<input type="text" title="Enter your name">

<!-- Valid: input with placeholder (not ideal but valid) -->
<input type="text" placeholder="Name">
