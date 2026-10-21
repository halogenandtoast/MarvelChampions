<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'

const { align = 'left' } = defineProps<{ align?: 'left' | 'right' }>()

const open = ref(false)
const root = ref<HTMLElement | null>(null)

const close = () => {
  open.value = false
}

// A menu closes when you press anywhere outside it or press Escape. A press, not a click:
// iOS sends no click for a tap on something that isn't clickable.
const onDocumentPointerDown = (event: PointerEvent) => {
  if (root.value && !root.value.contains(event.target as Node)) close()
}
const onKeydown = (event: KeyboardEvent) => {
  if (event.key === 'Escape') close()
}

onMounted(() => {
  document.addEventListener('pointerdown', onDocumentPointerDown)
  document.addEventListener('keydown', onKeydown)
})
onBeforeUnmount(() => {
  document.removeEventListener('pointerdown', onDocumentPointerDown)
  document.removeEventListener('keydown', onKeydown)
})
</script>

<template>
  <div ref="root" class="menu">
    <button type="button" :aria-expanded="open" @click="open = !open">
      <slot></slot>
      <svg viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
        <path fill-rule="evenodd" d="M5.23 7.21a.75.75 0 0 1 1.06.02L10 11.17l3.71-3.94a.75.75 0 1 1 1.08 1.04l-4.25 4.5a.75.75 0 0 1-1.08 0l-4.25-4.5a.75.75 0 0 1 .02-1.06Z" clip-rule="evenodd" />
      </svg>
    </button>
    <transition name="appear">
      <div v-if="open" class="menu-items" :class="align" @click="close">
        <slot name="items"></slot>
      </div>
    </transition>
  </div>
</template>

<style scoped>
.menu {
  position: relative;
  height: 100%;
}

button {
  background: none;
  border: 0;
  border-radius: 0;
  color: inherit;
  font: inherit;
  display: flex;
  height: 100%;
  padding: 5px 10px;
  gap: 5px;
  align-items: center;
  cursor: pointer;
}
button:hover { background: var(--hover-tint); }

svg { width: 20px; }

.menu-items {
  background-color: var(--menu-background);
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 5px;
  position: absolute;
  top: 100%;
  left: 0;
  min-width: 12rem;
  z-index: 10;
  display: flex;
  flex-direction: column;
  box-shadow: 0 8px 24px var(--shadow);
}

.menu-items.right { left: auto; right: 0; }

.appear-enter-active, .appear-leave-active { transition: opacity 0.2s ease-in-out; }
.appear-enter-from, .appear-leave-to { opacity: 0; }
</style>
