import type * as Kit from '@sveltejs/kit';

export interface PageData {
    title: string;
    items: string[];
}

export interface PageLoadEvent {
    params: Record<string, string>;
    url: URL;
    fetch: typeof fetch;
}

export type PageLoad = (event: PageLoadEvent) => Promise<PageData> | PageData;
