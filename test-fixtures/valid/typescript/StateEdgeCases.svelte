<script lang="ts">
    // No type annotation - should NOT be transformed
    let count = $state(0);
    
    // Already has generic - should NOT be transformed
    let items: string[] = $state<string[]>([]);
    
    // Union type with const keyword
    const mode: 'light' | 'dark' = $state('light');
    
    // Complex type annotation with generics
    let selected: Map<string, number> = $state(new Map());
    
    // Type annotation with object type
    let config: { name: string; value: number } = $state({ name: 'test', value: 1 });
</script>

<div>
    Count: {count}
    Mode: {mode === 'dark' ? 'Dark' : 'Light'}
    Items: {items.length}
</div>
