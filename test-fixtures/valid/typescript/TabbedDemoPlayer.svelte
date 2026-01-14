<script lang="ts">
        let activeTab: 'cli' | 'vscode' = $state('cli')
</script>

<div>
        <div class="flex justify-center">
                <button
                        class="px-4 py-2 font-mono text-sm transition-colors {activeTab === 'cli'
                                ? 'text-white bg-[#282828]'
                                : 'text-muted-foreground hover:bg-muted/20'}"
                        onclick={() => (activeTab = 'cli')}
                >
                        Terminal
                </button>
                <button
                        class="px-4 py-2 font-mono text-sm transition-colors {activeTab === 'vscode'
                                ? 'text-white bg-[#212325]'
                                : 'text-muted-foreground hover:bg-muted/20'}"
                        onclick={() => (activeTab = 'vscode')}
                >
                        VS Code
                </button>
        </div>
        <div class="relative mx-auto w-full overflow-hidden" style="aspect-ratio: 810 / 488">
                <div class={activeTab === 'cli' ? 'bg-[#282828]' : 'hidden'}>
                        content1
                </div>
                <div class={activeTab === 'vscode' ? 'h-full bg-[#212325] p-1 overflow-hidden' : 'hidden'}>
                        content2
                </div>
        </div>
</div>
