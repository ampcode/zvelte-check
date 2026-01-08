<!--
  TypeScript prop type mismatches.
  Tests incorrect prop types passed to components.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    // Component that accepts specific prop types
    interface ButtonProps {
        variant: 'primary' | 'secondary' | 'danger';
        size: 'sm' | 'md' | 'lg';
        disabled?: boolean;
        onClick: () => void;
    }

    interface UserCardProps {
        user: {
            id: number;
            name: string;
            email: string;
        };
        showEmail?: boolean;
    }

    interface ListProps<T> {
        items: T[];
        renderItem: (item: T) => string;
    }
</script>

<!-- Wrong literal type for variant prop -->
<!-- Expected: line 32, code: TS2322, message: Type '"invalid"' is not assignable to type -->
<Button variant="invalid" size="md" onClick={() => {}} />

<!-- Wrong type for size prop (number instead of string literal) -->
<!-- Expected: line 36, code: TS2322, message: Type 'number' is not assignable -->
<Button variant="primary" size={42} onClick={() => {}} />

<!-- Missing required prop (onClick) -->
<!-- Expected: line 40, code: TS2741, message: Property 'onClick' is missing -->
<Button variant="primary" size="md" />

<!-- Wrong type for boolean prop (string instead of boolean) -->
<!-- Expected: line 44, code: TS2322, message: Type 'string' is not assignable to type 'boolean' -->
<Button variant="primary" size="md" disabled="true" onClick={() => {}} />

<!-- Missing required nested property -->
<!-- Expected: line 48, code: TS2741, message: Property 'email' is missing -->
<UserCard user={{ id: 1, name: "Alice" }} />

<!-- Wrong type for nested property -->
<!-- Expected: line 52, code: TS2322, message: Type 'string' is not assignable to type 'number' -->
<UserCard user={{ id: "one", name: "Alice", email: "alice@test.com" }} />

<!-- Extra property not defined in interface -->
<!-- Expected: line 56, code: TS2353, message: Object literal may only specify known properties -->
<UserCard user={{ id: 1, name: "Alice", email: "a@b.com", role: "admin" }} />

<!-- Generic component with wrong item type -->
<!-- Expected: line 61, code: TS2345, message: Argument of type 'number' is not assignable -->
<List 
    items={[1, 2, 3]} 
    renderItem={(item: string) => item.toUpperCase()} 
/>
