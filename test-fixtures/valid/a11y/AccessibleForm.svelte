<!--
  Valid form component with full a11y compliance.
  Should produce zero diagnostics.
-->
<script lang="ts">
    interface Props {
        onsubmit?: (data: FormData) => void;
    }

    let { onsubmit }: Props = $props();

    let name = $state('');
    let email = $state('');
    let message = $state('');
    let errors: Record<string, string> = $state({});

    function handleSubmit(event: SubmitEvent) {
        event.preventDefault();
        const form = event.target as HTMLFormElement;
        const data = new FormData(form);
        onsubmit?.(data);
    }
</script>

<form onsubmit={handleSubmit} aria-labelledby="form-title">
    <h2 id="form-title">Contact Form</h2>

    <div class="field">
        <label for="name-input">Name <span aria-hidden="true">*</span></label>
        <input
            id="name-input"
            type="text"
            name="name"
            bind:value={name}
            required
            aria-required="true"
            aria-describedby={errors.name ? 'name-error' : undefined}
            aria-invalid={errors.name ? 'true' : undefined}
        />
        {#if errors.name}
            <span id="name-error" class="error" role="alert">{errors.name}</span>
        {/if}
    </div>

    <div class="field">
        <label for="email-input">Email <span aria-hidden="true">*</span></label>
        <input
            id="email-input"
            type="email"
            name="email"
            bind:value={email}
            required
            aria-required="true"
            aria-describedby={errors.email ? 'email-error' : undefined}
            aria-invalid={errors.email ? 'true' : undefined}
        />
        {#if errors.email}
            <span id="email-error" class="error" role="alert">{errors.email}</span>
        {/if}
    </div>

    <div class="field">
        <label for="message-input">Message</label>
        <textarea
            id="message-input"
            name="message"
            bind:value={message}
            rows="4"
        ></textarea>
    </div>

    <button type="submit">Send Message</button>
</form>

<style>
    form {
        max-width: 400px;
    }

    .field {
        margin-bottom: 1rem;
    }

    label {
        display: block;
        margin-bottom: 0.25rem;
        font-weight: 500;
    }

    input,
    textarea {
        width: 100%;
        padding: 0.5rem;
        border: 1px solid #ccc;
        border-radius: 4px;
    }

    .error {
        color: #c00;
        font-size: 0.875rem;
    }

    button {
        padding: 0.5rem 1rem;
        cursor: pointer;
    }
</style>
