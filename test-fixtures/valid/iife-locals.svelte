<script lang="ts">
// Regression test: Variables declared inside IIFEs should not be extracted
// to module scope. They are local to the IIFE function body.
let position = { left: 100, menuWidth: 425 };
</script>

<div style={`left: ${(() => {
  const menuWidth = 425;
  const leftPos = Math.min(position.left, 100);
  return leftPos;
})()}px`}>
  IIFE test
</div>
