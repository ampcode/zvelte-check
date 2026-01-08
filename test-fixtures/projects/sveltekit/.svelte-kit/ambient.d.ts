/// <reference types="svelte" />

declare module '$app/environment' {
    export const browser: boolean;
    export const dev: boolean;
    export const building: boolean;
    export const version: string;
}

declare module '$app/navigation' {
    export function goto(url: string | URL): Promise<void>;
    export function invalidate(url: string | URL): Promise<void>;
    export function invalidateAll(): Promise<void>;
    export function preloadData(url: string | URL): Promise<void>;
    export function preloadCode(...urls: string[]): Promise<void>;
}

declare module '$app/stores' {
    import { Readable } from 'svelte/store';
    export const page: Readable<{
        url: URL;
        params: Record<string, string>;
        route: { id: string | null };
        status: number;
        error: Error | null;
        data: Record<string, unknown>;
    }>;
    export const navigating: Readable<{
        from: { url: URL; params: Record<string, string> } | null;
        to: { url: URL; params: Record<string, string> } | null;
    } | null>;
    export const updated: Readable<boolean>;
}
