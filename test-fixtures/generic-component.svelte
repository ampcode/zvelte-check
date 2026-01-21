<script lang="ts" generics="T extends string = string">
    // Generic component with a type parameter used in props
    let { value }: { value: T } = $props();
</script>

{#snippet row(item: T)}
    <div>{item}</div>
{/snippet}

<div>
    {@render row(value)}
</div>
