<!--
  TypeScript type errors in script blocks.
  Tests basic type mismatches and missing properties.
-->
<script lang="ts">
    // Type mismatch: string assigned to number
    // Expected: line 8, code: TS2322, message: Type 'string' is not assignable to type 'number'
    let count: number = "not a number";

    // Type mismatch: number assigned to string
    // Expected: line 12, code: TS2322, message: Type 'number' is not assignable to type 'string'
    let name: string = 42;

    // Type mismatch: boolean assigned to string
    // Expected: line 16, code: TS2322, message: Type 'boolean' is not assignable to type 'string'
    let label: string = true;

    // Missing property on object type
    interface User {
        id: number;
        name: string;
        email: string;
    }

    // Expected: line 26, code: TS2741, message: Property 'email' is missing
    const user: User = { id: 1, name: "Alice" };

    // Invalid property on object type
    // Expected: line 30, code: TS2353, message: Object literal may only specify known properties
    const user2: User = { id: 2, name: "Bob", email: "bob@test.com", age: 25 };

    // Type mismatch in array
    // Expected: line 34, code: TS2322, message: Type 'string' is not assignable to type 'number'
    const numbers: number[] = [1, 2, "three"];

    // Function return type mismatch
    // Expected: line 39, code: TS2322, message: Type 'string' is not assignable to type 'number'
    function getValue(): number {
        return "wrong";
    }

    // Function parameter type mismatch
    function greet(name: string): void {
        console.log(`Hello, ${name}`);
    }
    // Expected: line 47, code: TS2345, message: Argument of type 'number' is not assignable
    greet(123);

    // Null/undefined errors with strict mode
    let maybeNull: string | null = null;
    // Expected: line 52, code: TS2531, message: Object is possibly 'null'
    console.log(maybeNull.length);
</script>

<p>Count: {count}</p>
