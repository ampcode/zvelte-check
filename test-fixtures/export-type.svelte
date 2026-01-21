<script lang="ts">
    export type MyProps = {
        name: string;
        count: number;
    }

    let { name, count }: MyProps = $props();
</script>

<div>{name}: {count}</div>
