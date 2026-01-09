import type { PageServerLoad, Actions } from './$types';

export const load: PageServerLoad = async ({ params, cookies }) => {
    const session = cookies.get('session');
    return {
        title: 'Home',
        items: ['Item 1', 'Item 2', 'Item 3'],
        session: session ?? null,
    };
};

export const actions: Actions = {
    default: async ({ request }) => {
        const data = await request.formData();
        const name = data.get('name');
        return { success: true, name };
    },
};
