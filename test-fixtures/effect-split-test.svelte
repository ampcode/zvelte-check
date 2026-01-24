<script lang="ts">
	import type { EvalResult } from '../../../../src/types'
	import ModelResultColumn from './ModelResultColumn.svelte'

	interface DatasetItem {
		id: number
		repo_url: string
		repo_sha: string
		query: string
		// ... other fields we don't strictly need here but good to match
	}

	let { models, dataset }: { models: string[]; dataset: DatasetItem[] } = $props()

	let modelAId = $state('')
	let modelBId = $state('')
	let resultsA = $state<EvalResult[]>([])
	let resultsB = $state<EvalResult[]>([])
	let loadingA = $state(false)
	let loadingB = $state(false)
	let errorA = $state<string | null>(null)
	let errorB = $state<string | null>(null)
	let selectedTaskIndex = $state<number | null>(null)

	function handleKeydown(event: KeyboardEvent) {
		if (!commonTasks.length || selectedTaskIndex === null) return

		const currentIndex = commonTasks.findIndex((t) => t.index === selectedTaskIndex)
		if (currentIndex === -1) return

		if (event.key === 'ArrowDown') {
			event.preventDefault()
			const nextIndex = Math.min(currentIndex + 1, commonTasks.length - 1)
			selectedTaskIndex = commonTasks[nextIndex].index
			scrollToTask(selectedTaskIndex)
		} else if (event.key === 'ArrowUp') {
			event.preventDefault()
			const prevIndex = Math.max(currentIndex - 1, 0)
			selectedTaskIndex = commonTasks[prevIndex].index
			scrollToTask(selectedTaskIndex)
		}
	}

	function scrollToTask(index: number) {
		// Simple scroll implementation - in a real app we might want more robust scrolling
		const el = document.getElementById(`task-btn-${index}`)
		el?.scrollIntoView({ block: 'nearest' })
	}

	$effect(() => {
		if (models.length > 0 && !modelAId && !modelBId) {
			// Default to the last 2 models if available (assuming newer at the end or just arbitrary)
			// Actually, let's pick the first two as they might be the most relevant or sorted by caller
			if (models.length >= 1) handleModelChange('A', models[0])
			if (models.length >= 2) handleModelChange('B', models[1])
		}
	})

	// When models change, fetch results
	async function fetchResults(modelId: string, target: 'A' | 'B') {
		if (!modelId) return
		if (target === 'A') {
			loadingA = true
			errorA = null
		} else {
			loadingB = true
			errorB = null
		}
	}
</script>

<p>Test</p>
