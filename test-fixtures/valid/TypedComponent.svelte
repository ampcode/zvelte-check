<!--
  Valid component with comprehensive TypeScript types.
  Should produce zero diagnostics.
-->
<script lang="ts">
    import type { Snippet } from 'svelte';

    type Status = 'idle' | 'loading' | 'success' | 'error';

    interface User {
        id: number;
        name: string;
        email: string;
        role: 'admin' | 'user' | 'guest';
    }

    interface ApiResponse<T> {
        data: T;
        status: number;
        message?: string;
    }

    interface Props {
        user: User;
        status?: Status;
        onstatuschange?: (status: Status) => void;
        children?: Snippet;
        fallback?: Snippet<[{ error: Error }]>;
    }

    let { user, status = 'idle', onstatuschange, children, fallback }: Props = $props();

    let response: ApiResponse<User[]> | null = $state(null);
    let error: Error | null = $state(null);

    const statusColors: Record<Status, string> = {
        idle: '#888',
        loading: '#007bff',
        success: '#28a745',
        error: '#dc3545'
    };

    let statusColor = $derived(statusColors[status]);

    async function fetchUsers(): Promise<void> {
        onstatuschange?.('loading');
        try {
            const res = await fetch('/api/users');
            const data: ApiResponse<User[]> = await res.json();
            response = data;
            onstatuschange?.('success');
        } catch (e) {
            error = e instanceof Error ? e : new Error('Unknown error');
            onstatuschange?.('error');
        }
    }

    function getRoleBadge(role: User['role']): string {
        const badges: Record<User['role'], string> = {
            admin: '👑',
            user: '👤',
            guest: '👻'
        };
        return badges[role];
    }
</script>

<div class="user-card">
    <header>
        <h2>{user.name} {getRoleBadge(user.role)}</h2>
        <span class="status" style="color: {statusColor}">{status}</span>
    </header>

    <p>{user.email}</p>

    <button type="button" onclick={fetchUsers}>
        Load Users
    </button>

    {#if status === 'error' && error && fallback}
        {@render fallback({ error })}
    {:else if children}
        {@render children()}
    {/if}

    {#if response?.data}
        <ul>
            {#each response.data as u (u.id)}
                <li>{u.name} ({u.email})</li>
            {/each}
        </ul>
    {/if}
</div>

<style>
    .user-card {
        padding: 1rem;
        border: 1px solid #ddd;
        border-radius: 8px;
    }

    header {
        display: flex;
        justify-content: space-between;
        align-items: center;
    }

    .status {
        font-size: 0.875rem;
        text-transform: uppercase;
    }

    ul {
        margin-top: 1rem;
        padding-left: 1.5rem;
    }
</style>
