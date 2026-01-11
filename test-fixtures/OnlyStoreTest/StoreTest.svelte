<script lang="ts">
	import type { Readable } from 'svelte/store'
	import { readable } from 'svelte/store'

	type AuthenticatedStatus = { user: { name: string } }
	type ErrorStatus = { error: { message: string } }
	type ServerStatus = AuthenticatedStatus | ErrorStatus | 'pending'

	function isAuthenticatedStatus(status: ServerStatus): status is AuthenticatedStatus {
		return typeof status === 'object' && 'user' in status
	}

	function observableToReadable<T>(): Readable<T | undefined> {
		return readable<T | undefined>(undefined)
	}

	let serverStatus = observableToReadable<ServerStatus>()
	
	// This should work after type narrowing with isAuthenticatedStatus
	let isAuth = $derived(
		$serverStatus && isAuthenticatedStatus($serverStatus) 
			? $serverStatus.user.name 
			: undefined
	)
</script>
