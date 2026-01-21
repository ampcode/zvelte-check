<script lang="ts">
const code = `
import { something } from 'fake';
const x = 1;
`;

const nested = `template with \`nested ${`import { x } from 'y'`}\``;

console.log(code);
</script>

<p>Testing template literal import extraction</p>
