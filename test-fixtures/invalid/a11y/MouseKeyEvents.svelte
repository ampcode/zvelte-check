<!-- Test fixtures for mouse/keyboard event pairing a11y rules -->

<!-- a11y-mouse-events-have-key-events: mouseenter without focus -->
<!-- Expected: line 5, code: a11y-mouse-events-have-key-events -->
<div on:mouseenter={() => {}}>Mouse enter without focus</div>

<!-- a11y-mouse-events-have-key-events: mouseleave without blur -->
<!-- Expected: line 9, code: a11y-mouse-events-have-key-events -->
<div on:mouseleave={() => {}}>Mouse leave without blur</div>

<!-- a11y-mouse-events-have-key-events: mouseover without focus -->
<!-- Expected: line 13, code: a11y-mouse-events-have-key-events -->
<div on:mouseover={() => {}}>Mouse over without focus</div>

<!-- a11y-mouse-events-have-key-events: mouseout without blur -->
<!-- Expected: line 17, code: a11y-mouse-events-have-key-events -->
<div on:mouseout={() => {}}>Mouse out without blur</div>

<!-- a11y-mouse-events-have-key-events: both mouse events without keyboard -->
<!-- Expected: line 21, code: a11y-mouse-events-have-key-events -->
<!-- Expected: line 21, code: a11y-mouse-events-have-key-events -->
<div on:mouseenter={() => {}} on:mouseleave={() => {}}>Both mouse events</div>

<!-- Valid: mouseenter with focus -->
<div on:mouseenter={() => {}} on:focus={() => {}}>Has focus handler</div>

<!-- Valid: mouseleave with blur -->
<div on:mouseleave={() => {}} on:blur={() => {}}>Has blur handler</div>

<!-- Valid: mouseover with focus -->
<div on:mouseover={() => {}} on:focus={() => {}}>Has focus handler</div>

<!-- Valid: mouseout with blur -->
<div on:mouseout={() => {}} on:blur={() => {}}>Has blur handler</div>

<!-- Valid: all paired correctly -->
<div 
  on:mouseenter={() => {}} 
  on:mouseleave={() => {}} 
  on:focus={() => {}} 
  on:blur={() => {}}>
  All paired
</div>
