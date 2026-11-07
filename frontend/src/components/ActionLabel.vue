<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, useTemplateRef, watch, nextTick } from 'vue'

const { text, name = '' } = defineProps<{ text: string; name?: string }>()
const short = useTemplateRef<HTMLElement>('short')
const named = useTemplateRef<HTMLElement>('named')
const heights = ref<{ short: number; named: number } | null>(null)
const expands = computed(() => name && name !== text)
const measure = () => {
  if (short.value && named.value) heights.value = { short: short.value.offsetHeight, named: named.value.offsetHeight }
}
let observer: ResizeObserver | undefined
const observe = () => {
  observer?.disconnect()
  if (short.value) observer?.observe(short.value)
  if (named.value) observer?.observe(named.value)
  measure()
}
onMounted(() => { observer = new ResizeObserver(measure); observe() })
watch(() => [text, name], async () => { await nextTick(); observe() })
onBeforeUnmount(() => observer?.disconnect())
</script>

<template>
  <span v-if="expands" class="action-label" :style="heights ? { '--short-height': `${heights.short}px`, '--named-height': `${heights.named}px` } : undefined">
    <span ref="short" class="short" aria-hidden="true">{{ text }}</span>
    <span ref="named" class="named">{{ name }}</span>
  </span>
  <span v-else>{{ text }}</span>
</template>

<style scoped>
.action-label {
  position: relative;
  display: inline-block;
  width: 100%;
  height: var(--short-height, auto);
  overflow: hidden;
  vertical-align: bottom;
  transition: height 180ms ease;
}
.short, .named {
  display: block;
  width: 100%;
  white-space: normal;
  transition: opacity 130ms ease, transform 180ms ease;
}
.named { position: absolute; top: 0; left: 0; opacity: 0; transform: translateY(5px); }
:global(button:hover .action-label), :global(button:focus-visible .action-label) { height: var(--named-height, auto); }
:global(button:hover .action-label .short), :global(button:focus-visible .action-label .short) { opacity: 0; transform: translateY(-5px); }
:global(button:hover .action-label .named), :global(button:focus-visible .action-label .named) { opacity: 1; transform: translateY(0); }
@media (prefers-reduced-motion: reduce) {
  .action-label, .short, .named { transition: none; }
}
</style>
