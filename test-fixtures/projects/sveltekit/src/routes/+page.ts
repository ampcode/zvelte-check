import type { PageLoad } from './$types';

export const load: PageLoad = async ({ fetch }) => {
    return {
        title: 'Home',
        items: ['Item 1', 'Item 2', 'Item 3']
    };
};
