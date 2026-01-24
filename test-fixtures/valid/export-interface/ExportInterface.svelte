<!-- Regression test: export interface and export type in instance scripts should be hoisted.
     Previously, "export interface" and "export type" caused "Modifiers cannot appear here"
     errors because they weren't hoisted to module level like regular interfaces and types.
     Svelte 5 supports exported types in instance scripts for external consumption. -->
<script lang="ts">
  interface InternalType {
    value: string
  }

  export interface ExportedInterface {
    id: string
    data: InternalType
  }

  export type ExportedType = {
    name: string
    count: number
  }

  let { item }: { item: ExportedInterface } = $props()
</script>

<p>{item.id}</p>
