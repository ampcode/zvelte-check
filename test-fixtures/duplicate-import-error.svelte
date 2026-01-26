<script lang="ts">
  // Test: when module doesn't exist, should report ONE error for the module,
  // not multiple errors for each imported specifier.
  // Expected: 1 error at the module path './nonexistent-module'
  // Bug: zvelte-check was reporting error at columns for 'foo' AND 'bar' AND module path
  import { foo, bar, baz } from './nonexistent-module';

  // Use them to avoid unused variable warnings
  console.log(foo, bar, baz);
</script>

<p>Test duplicate import errors</p>
