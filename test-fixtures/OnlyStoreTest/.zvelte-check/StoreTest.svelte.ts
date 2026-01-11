// Generated from test-fixtures/OnlyStoreTest/StoreTest.svelte

import { SvelteComponentTyped } from "svelte";
import type { Snippet } from "svelte";

// Svelte 5 rune type stubs
declare function $state<T = undefined>(initial?: T): T;
declare namespace $state {
  function raw<T>(initial: T): T;
  function snapshot<T>(state: T): T;
}
declare function $derived<T>(expr: T): T;
declare namespace $derived {
  function by<T>(fn: () => T): T;
}
declare function $effect(fn: () => void | (() => void)): void;
declare namespace $effect {
  function pre(fn: () => void | (() => void)): void;
  function tracking(): boolean;
  function root(fn: () => void | (() => void)): () => void;
}
declare function $props<T = $$Props>(): T;
declare function $bindable<T>(initial?: T): T;
declare function $inspect<T>(...values: T[]): { with: (fn: (type: 'init' | 'update', ...values: T[]) => void) => void };
declare function $host<T extends HTMLElement>(): T;

// Svelte store auto-subscription stub
// $storeName syntax in Svelte auto-subscribes to the store
// This function extracts the value type from a store's subscribe method
type SvelteStore<T> = { subscribe: (run: (value: T) => any, invalidate?: any) => any };
declare function __svelte_store_get<T>(store: SvelteStore<T>): T;
declare function __svelte_store_get<Store extends SvelteStore<any> | undefined | null>(store: Store): Store extends SvelteStore<infer T> ? T : Store;

// <script>

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

// Store subscriptions
var $serverStatus = __svelte_store_get(serverStatus);


// Component typing
export interface $$Props {
}

export interface $$Slots {
  default: {};
}

export type $$Events = Record<string, any>;

export default class __SvelteComponent__ extends SvelteComponentTyped<$$Props, $$Events, $$Slots> {}
