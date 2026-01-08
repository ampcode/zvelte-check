<script lang="ts">
    interface Props {
        label: string;
        disabled?: boolean;
        onclick?: () => void;
    }

    let { label, disabled = false, onclick }: Props = $props();
</script>

<button {disabled} {onclick}>
    {label}
</button>

<style>
    button {
        padding: 0.5rem 1rem;
        border-radius: 4px;
        cursor: pointer;
    }

    button:disabled {
        cursor: not-allowed;
        opacity: 0.5;
    }
</style>
