// Simulates svelte2tsx-generated types for an external package
import type { SvelteComponent } from 'svelte';

type Props = {
  value: string;
  onchange?: (value: string) => void;
};

interface $$__sveltets_2_IsomorphicComponent<P> {
  new (options: { props?: P }): SvelteComponent<P>;
  (internal: unknown, props: P): {};
}

declare const Child: $$__sveltets_2_IsomorphicComponent<Props>;
export default Child;
