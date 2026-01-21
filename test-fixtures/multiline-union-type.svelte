<script lang="ts">
type Part =
  | { type: 'a'; value: string }
  | { type: 'b'; count: number }
  | { type: 'c' }

const x: Part = { type: 'a', value: 'hello' }
</script>

<p>Testing multi-line union type</p>
