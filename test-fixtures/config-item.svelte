<script lang="ts">
	let {
		name,
		type,
		defaultValue,
		options,
		description,
		platform,
		example,
		children,
	}: {
		name: string
		type: string
		defaultValue?: string | number | boolean
		options?: string[]
		description: string
		platform?: 'vscode' | 'cli' | 'all'
		example?: string
		children?: import('svelte').Snippet
	} = $props()

	// Convert description with `code` blocks and links to HTML
	const formattedDescription = $derived(
		description
			// First handle links (before code blocks to prevent double processing)
			.replace(
				/`<a href='([^']+)'>([^<]+)<\/a>`/g,
				'<a href="$1" class="hover:underline">$2</a>',
			)
			// Then handle code blocks (excluding links)
			.replace(
				/`([^`]+)`/g,
				'<code class="bg-[color-mix(in_srgb,var(--text-color)_8%,transparent)] text-[color-mix(in_srgb,var(--text-color)_90%,transparent)] px-1 py-0.5 rounded text-xs font-mono font-medium">$1</code>',
			),
	)
</script>

<div class="bg-transparent overflow-auto w-full p-6 rounded-xl font-serif border">
	<div class="flex items-start justify-between mb-2 gap-2">
		<div class="flex items-center gap-2 flex-wrap">
			<span class="text-base font-semibold font-mono">{name}</span>
			<code
				class="bg-[color-mix(in_srgb,var(--text-color)_8%,transparent)] text-[color-mix(in_srgb,var(--text-color)_90%,transparent)] px-1 py-0.5 rounded font-mono"
				>{type}</code
			>
			{#if defaultValue !== undefined}
				<code
					class="bg-[color-mix(in_srgb,var(--text-color)_8%,transparent)] text-[color-mix(in_srgb,var(--text-color)_90%,transparent)] px-1 py-0.5 rounded font-mono"
				>
					default: {typeof defaultValue === 'string' ? `"${defaultValue}"` : defaultValue}
				</code>
			{/if}
		</div>
		{#if platform}
			<span class="whitespace-nowrap text-muted-foreground">
				{#if platform === 'all'}
					VS Code & CLI
				{:else if platform === 'vscode'}
					VS Code
				{:else if platform === 'cli'}
					CLI
				{/if}
			</span>
		{/if}
	</div>
	<div class="text-lg leading-5 text-[var(--text-color)]">
		<!-- eslint-disable-next-line svelte/no-at-html-tags -->
		{@html formattedDescription}
	</div>
	{#if example}
		<div class="mt-2">
			<div class="text-lg font-semibold mb-1">Example:</div>
			<code
				class="block bg-[color-mix(in_srgb,var(--text-color)_18%,transparent)] text-[color-mix(in_srgb,var(--text-color)_90%,transparent)] px-3 py-2 rounded font-mono text-sm leading-relaxed!"
				>{example}</code
			>
		</div>
	{/if}
	{#if options}
		<div class="mt-2 font-semibold">
			<span class="text-lg">Options:</span>
			{#each options as option, i (option)}<code
					class="bg-[color-mix(in_srgb,var(--text-color)_8%,transparent)] text-[color-mix(in_srgb,var(--text-color)_90%,transparent)] px-1 py-0.5 rounded font-mono"
					>{option}</code
				>{#if i < options.length - 1},
				{/if}{/each}
		</div>
	{/if}
	{#if children}
		<div class="mt-2 font-serif">
			{@render children()}
		</div>
	{/if}
</div>
