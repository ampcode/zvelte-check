<!--
  CSS style: directive syntax.
  Tests that style: directives are properly parsed.
  Should produce zero diagnostics.
-->
<script lang="ts">
    let color = $state('red');
    let fontSize = $state('16px');
    let isActive = $state(true);
    let opacity = $state(1);
</script>

<!-- Shorthand style directive -->
<div style:color>
    Shorthand (uses variable named 'color')
</div>

<!-- Style directive with value -->
<div style:color={color}>
    Dynamic color
</div>

<!-- Style directive with expression -->
<div style:opacity={isActive ? 1 : 0.5}>
    Conditional opacity
</div>

<!-- Style directive with string value -->
<div style:font-size="20px">
    Static font size
</div>

<!-- Multiple style directives -->
<div 
    style:color={color}
    style:font-size={fontSize}
    style:opacity={opacity}
>
    Multiple styles
</div>

<!-- Mixed with regular style attribute -->
<div 
    style="padding: 1rem; margin: 0.5rem;"
    style:color={color}
    style:border-radius="4px"
>
    Mixed styles
</div>

<!-- Template literal in style directive -->
<div style:transform={`translateX(${opacity * 100}px)`}>
    Transform
</div>

<!-- Self-closing with style directive -->
<hr style:border-color={color} />

<style>
    div {
        padding: 0.5rem;
        margin: 0.25rem;
        border: 1px solid #ccc;
    }
</style>
