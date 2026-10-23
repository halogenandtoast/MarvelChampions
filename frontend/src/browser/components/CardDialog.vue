<script setup lang="ts">
/**
 * The selected card, in a modal.
 *
 * A native `<dialog>` rather than a hand-rolled overlay: it traps focus, closes
 * on Escape, renders on the top layer above everything, and gives us a real
 * backdrop, all without a library.
 *
 * Keyboard: left and right step through the result list, up and down through
 * the faces of the card in front of you.
 */
import { computed, ref, watch } from 'vue'
import CardDetail from './CardDetail.vue'
import type { Card } from '@browser/types/card'
import { useCardStore } from '@browser/stores/cards'

const { cards, index } = defineProps<{
  /** The current result list, so the arrows can walk it. */
  cards: Card[]
  /** Position in that list, or -1 when nothing is selected. */
  index: number
}>()

const emit = defineEmits<{ 'update:index': [value: number]; close: [] }>()

const store = useCardStore()
const dialog = ref<HTMLDialogElement | null>(null)
const imageIndex = ref(0)

const card = computed(() => (index >= 0 ? (cards[index] ?? null) : null))
const hasPrev = computed(() => index > 0)
const hasNext = computed(() => index >= 0 && index < cards.length - 1)

// Opening and closing is driven by the selection, not by calling showModal from
// a click handler, so a card arrived at through the URL opens too.
watch(
  () => card.value,
  (current, previous) => {
    const el = dialog.value
    if (!el) return
    if (current && !el.open) el.showModal()
    else if (!current && el.open) el.close()
    if (current?.code !== previous?.code) imageIndex.value = 0
  },
  { flush: 'post' },
)

function step(by: number): void {
  const next = index + by
  if (next >= 0 && next < cards.length) emit('update:index', next)
}

function stepImage(by: number): void {
  const faces = !card.value?.linkedTo ? 1 : store.thirdFace(card.value) ? 3 : 2
  if (faces < 2) return
  // Wraps: with two faces, either arrow flips the card, which is what the
  // gesture means here; a fold-out identity's third face follows the second.
  imageIndex.value = (imageIndex.value + by + faces) % faces
}

function onKeydown(event: KeyboardEvent): void {
  // Let the browser have the keys while a field inside the dialog has focus.
  const target = event.target as HTMLElement | null
  if (target?.closest('input, textarea, select')) return

  switch (event.key) {
    case 'ArrowLeft':
      event.preventDefault()
      step(-1)
      break
    case 'ArrowRight':
      event.preventDefault()
      step(1)
      break
    case 'ArrowUp':
      event.preventDefault()
      stepImage(-1)
      break
    case 'ArrowDown':
      event.preventDefault()
      stepImage(1)
      break
  }
}

/** A click on the dialog element itself landed on the backdrop, not the card. */
function onClick(event: MouseEvent): void {
  if (event.target === dialog.value) emit('close')
}
</script>

<template>
  <dialog
    ref="dialog"
    class="dialog"
    :aria-label="$t('browser.dialog.label')"
    @cancel.prevent="emit('close')"
    @close="emit('close')"
    @keydown="onKeydown"
    @click="onClick"
  >
    <div v-if="card" class="dialog__frame">
      <header class="dialog__bar">
        <div class="dialog__nav">
          <button
            type="button"
            class="button button--ghost"
            :disabled="!hasPrev"
            :title="$t('browser.dialog.previousCardTitle')"
            @click="step(-1)"
          >
            <span aria-hidden="true">←</span>
            <span class="visually-hidden">{{ $t('browser.dialog.previousCard') }}</span>
          </button>
          <span class="dialog__position">{{ $t('browser.dialog.position', { index: index + 1, total: cards.length }) }}</span>
          <button
            type="button"
            class="button button--ghost"
            :disabled="!hasNext"
            :title="$t('browser.dialog.nextCardTitle')"
            @click="step(1)"
          >
            <span aria-hidden="true">→</span>
            <span class="visually-hidden">{{ $t('browser.dialog.nextCard') }}</span>
          </button>
        </div>

        <p class="dialog__hint">
          <kbd>←</kbd><kbd>→</kbd> {{ $t('browser.dialog.hintCards') }}
          <template v-if="card.linkedTo"> · <kbd>↑</kbd><kbd>↓</kbd> {{ $t('browser.dialog.hintFaces') }} </template>
          · <kbd>Esc</kbd> {{ $t('browser.dialog.hintClose') }}
        </p>

        <button type="button" class="button button--ghost" :title="$t('browser.common.close')" @click="emit('close')">
          <span aria-hidden="true">✕</span>
          <span class="visually-hidden">{{ $t('browser.common.close') }}</span>
        </button>
      </header>

      <div class="dialog__body">
        <CardDetail v-model:image-index="imageIndex" :card="card" />
      </div>
    </div>
  </dialog>
</template>

<style scoped>
.dialog {
  width: min(58rem, 100vw - 2rem);
  max-width: none;
  max-height: min(52rem, 100dvh - 2rem);
  padding: 0;
  border: 1px solid var(--c-border-strong);
  border-radius: var(--radius-lg);
  background: var(--c-bg);
  color: var(--c-text);
  box-shadow: var(--shadow-lg);
  overflow: hidden;
}

.dialog::backdrop {
  background: hsl(0 0% 0% / 0.6);
  backdrop-filter: blur(2px);
}

.dialog__frame {
  display: flex;
  flex-direction: column;
  max-height: inherit;
}

.dialog__bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--space-3);
  padding: var(--space-2) var(--space-3);
  border-bottom: 1px solid var(--c-border);
  background: var(--c-surface);
}

.dialog__nav {
  display: flex;
  align-items: center;
  gap: var(--space-2);
}

.dialog__position {
  color: var(--c-text-muted);
  font-size: var(--text-sm);
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}

.dialog__hint {
  display: flex;
  align-items: center;
  gap: 0.25em;
  color: var(--c-text-faint);
  font-size: var(--text-xs);
}

kbd {
  display: inline-grid;
  place-items: center;
  min-width: 1.4em;
  padding: 0.05em 0.3em;
  border: 1px solid var(--c-border-strong);
  border-radius: var(--radius-sm);
  background: var(--c-surface-2);
  font-family: inherit;
  font-size: 0.95em;
  line-height: 1.4;
}

.dialog__body {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  overscroll-behavior: contain;
  padding: var(--space-4);
}

@media (width < 40rem) {
  .dialog {
    width: 100vw;
    max-height: 100dvh;
    border-radius: 0;
    border: none;
  }

  .dialog__hint {
    display: none;
  }
}
</style>
