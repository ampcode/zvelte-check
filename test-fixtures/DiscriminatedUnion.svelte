<script lang="ts">
	type MarketplaceInfo = {
		type: 'marketplace';
		configureURL: string;
		instructions: string | null;
		marketplaceURL: string | null;
	};
	
	type CliInfo = {
		type: 'cli';
		install: { instructions?: string; label?: string };
		configure: { pluginUrl?: string; instructions?: string };
	};
	
	type ClientInstallInfo = MarketplaceInfo | CliInfo;
	
	let { info }: { info: ClientInstallInfo } = $props();
	
	type ChildProps = { props: Record<string, any>; open: boolean };
</script>

<!-- Discriminated union narrowing: accessing variant-specific property -->
{#if info.type === 'marketplace'}
	<span>{info.configureURL}</span>
	<!-- Nested if within discriminated union -->
	{#if info.instructions || info.marketplaceURL}
		<span>Has instructions or URL</span>
		<!-- Snippet inside if blocks - this is the real-world pattern -->
		{#snippet child({ props, open }: ChildProps)}
			{#if open}
				{#if info.instructions}
					<span>{info.instructions}</span>
				{/if}
			{/if}
		{/snippet}
	{/if}
{:else if info.type === 'cli'}
	<span>{info.install.instructions}</span>
	{#if info.configure.pluginUrl}
		<span>{info.configure.pluginUrl}</span>
	{/if}
{/if}
