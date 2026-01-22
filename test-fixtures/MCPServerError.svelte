<script lang="ts">
    let { error }: { error: Error } = $props()
</script>
<div>{error.message}</div>
