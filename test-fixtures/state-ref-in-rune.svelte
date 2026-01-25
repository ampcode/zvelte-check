<script lang="ts">
    let { data } = $props()
    
    // $state(expr) captures initial value - should warn
    let currentThreadID = $state(data.thread.id)
    
    // $derived() is reactive, no warning needed
    let hoursInput = $derived(data.hours)
    
    // Inside closure - reactive, no warning
    let reactiveHours = $derived.by(() => data.hours)
</script>

<p>{hoursInput} {currentThreadID}</p>
