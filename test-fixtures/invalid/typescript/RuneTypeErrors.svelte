<!--
  Svelte 5 runes with TypeScript type errors.
  Tests $state, $derived, $effect, $props with type mismatches.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    interface Props {
        initialCount: number;
        multiplier?: number;
        children?: Snippet;
    }

    let { initialCount, multiplier = 1, children }: Props = $props();

    // $state with wrong type assignment
    // Expected: line 17, code: TS2322, message: Type 'string' is not assignable to type 'number'
    let count: number = $state("zero");

    // $state array with wrong element type
    // Expected: line 21, code: TS2322, message: Type 'string' is not assignable to type 'number'
    let numbers: number[] = $state([1, 2, "three"]);

    // $derived with type mismatch in expression
    // Expected: line 25, code: TS2365, message: Operator '*' cannot be applied
    let doubled: number = $derived(count * "2");

    // $derived.by with wrong return type
    // Expected: line 30, code: TS2322, message: Type 'string' is not assignable to type 'number'
    let total: number = $derived.by(() => {
        return "not a number";
    });

    // $effect with non-void return (should return void or cleanup function)
    $effect(() => {
        // Expected: line 38, code: TS2322, message: Type 'string' is not assignable
        return "invalid cleanup";
    });

    // Using $bindable outside of $props destructuring
    // Expected: line 43, code: TS2304, message: Cannot find name '$bindable'
    let value = $bindable(0);

    // Type error in $props destructuring default
    // Expected: line 48, code: TS2322, message: Type 'string' is not assignable to type 'number'
    interface BadProps {
        count: number;
    }
    let { count: badCount = "default" }: BadProps = $props();

    // Accessing non-existent property from $props
    interface LimitedProps {
        name: string;
    }
    let { name, age }: LimitedProps = $props();
    // Expected: line 56, code: TS2339, message: Property 'age' does not exist

    // Wrong type in $state.raw
    // Expected: line 60, code: TS2322, message: Type 'string' is not assignable to type 'number'
    let rawValue: number = $state.raw("not raw number");

    // $inspect with wrong callback signature
    // Expected: line 65, code: TS2345, message: Argument of type
    $inspect(count).with((wrongType: boolean) => {
        console.log(wrongType);
    });
</script>

<p>Count: {count}</p>
{#if children}
    {@render children()}
{/if}
