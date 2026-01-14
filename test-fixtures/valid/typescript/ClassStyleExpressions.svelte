<script lang="ts">
  let isActive = $state(false);
  let size = $state('lg');
  let color = $state('#ff0000');
</script>

<!-- Multi-line ternary in class attribute -->
<button
  class="flex items-center gap-1.5 {isActive
    ? 'bg-primary/10 text-foreground'
    : 'hover:bg-muted/50 text-muted-foreground'}"
>
  Click me
</button>

<!-- Complex expression in style attribute -->
<div
  style="background-color: {isActive ? color : 'transparent'}; padding: {size === 'lg' ? '20px' : '10px'}"
>
  Content
</div>

<!-- Nested ternary in class -->
<span
  class="{isActive 
    ? (size === 'lg' ? 'large-active' : 'small-active')
    : 'inactive'}"
>
  Nested
</span>

<!-- Multiple expressions in one class attribute -->
<div
  class="base {isActive ? 'active' : ''} {size === 'lg' ? 'large' : 'small'}"
>
  Multiple
</div>
