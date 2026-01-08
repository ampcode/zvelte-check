<!--
  TypeScript import errors.
  Tests missing modules, missing exports, and invalid imports.
-->
<script lang="ts">
    // Missing module
    // Expected: line 8, code: TS2307, message: Cannot find module './NonExistent.svelte'
    import Missing from './NonExistent.svelte';

    // Missing named export
    // Expected: line 12, code: TS2305, message: Module '"./existing"' has no exported member 'missing'
    import { missing } from './existing';

    // Missing default export (when module has no default)
    // Expected: line 16, code: TS1192, message: Module '"./types"' has no default export
    import Default from './types';

    // Importing non-existent npm package
    // Expected: line 20, code: TS2307, message: Cannot find module 'nonexistent-package'
    import something from 'nonexistent-package';

    // Type-only import of value
    // Expected: line 24, code: TS1361, message: 'SomeClass' is a type and cannot be imported
    import type { SomeClass } from './classes';
    const instance = new SomeClass();

    // Importing from directory without index
    // Expected: line 29, code: TS2307, message: Cannot find module './utils/'
    import { helper } from './utils/';
</script>

<div>
    <Missing />
</div>
