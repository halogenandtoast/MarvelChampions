<script setup lang="ts">
// A deck, face down with its count, or a discard pile, its top card showing; a discard
// pile opens to show every card in it, where a choice about one of them can be made.
import { computed, useTemplateRef } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useChoices } from '../composables/useChoices'
import type { Back } from '../cards'
import type { CardView } from '../server'
import BoardCard from './BoardCard.vue'
import CardFace from './CardFace.vue'

const { label, back = null, count = null, cards = null, pile = null } = defineProps<{
  label: string
  /** what cards flying to and from it look for: deck-encounter, discard-<player> and so on */
  pile?: string | null
  /** face down: a deck */
  back?: Back | null
  count?: number | null
  /** face up: a discard pile, newest last */
  cards?: readonly CardView[] | null
}>()

const overlay = useCardOverlay()
const { byTarget } = useChoices()
const dialog = useTemplateRef<HTMLDialogElement>('dialog')
const total = computed(() => count ?? cards?.length ?? 0)
const top = computed(() => (cards?.length ? cards[cards.length - 1] : null))
// a choice about a card in it lights the pile, so the player knows to look
const active = computed(() => (cards ?? []).some((c) => byTarget.value.has(c.id)))
const open = () => {
  if (!cards?.length) return
  overlay.hide()
  dialog.value?.showModal()
}
const close = () => dialog.value?.close()
</script>

<template>
  <div class="pile" :class="{ empty: total === 0, active, openable: cards?.length }" :data-pile="pile ?? undefined">
    <button
      type="button"
      class="stack"
      :disabled="!cards?.length"
      :title="label"
      @click="open"
      @pointerenter="$event.pointerType === 'mouse' && top && overlay.show({ code: top.code, name: top.name }, $event.currentTarget as HTMLElement)"
      @pointerleave="overlay.hide()"
    >
      <CardFace v-if="back" :back="back" />
      <CardFace v-else-if="top" :code="top.code" :name="top.name" />
      <span v-else class="hollow" />
      <span class="count">{{ total }}</span>
    </button>
    <span class="label">{{ label }}</span>
    <dialog ref="dialog" class="pile-dialog" @click="$event.target === dialog && close()">
      <header>
        <h2>{{ label }} <small>{{ total }}</small></h2>
        <button type="button" class="btn ghost" @click="close">{{ $t('common.close') }}</button>
      </header>
      <div class="cards">
        <BoardCard v-for="card in [...(cards ?? [])].reverse()" :id="card.id" :key="card.id" :code="card.code" :name="card.name" :morph="false" />
      </div>
    </dialog>
  </div>
</template>

<style scoped>
.pile { --w: var(--card-width); display: flex; flex-direction: column; align-items: center; gap: 4px; flex: none; }
.stack {
  position: relative;
  width: var(--w);
  aspect-ratio: var(--card-aspect);
  padding: 0;
  border: 0;
  border-radius: calc(var(--w) * 0.07);
  background: none;
  box-shadow: 2px 2px 0 -1px var(--ink-4), 4px 4px 0 -2px var(--ink-5), 0 4px 10px rgb(0 0 0 / 0.5);
  cursor: default;
}
.pile.openable .stack { cursor: pointer; }
.pile.empty .stack { box-shadow: none; }
.stack :deep(.card-face) { border-radius: inherit; }
.pile.active .stack { box-shadow: 0 0 0 2px var(--interactive), 0 0 14px var(--interactive-glow); }
.hollow { display: block; width: 100%; height: 100%; border: 1px dashed var(--line-strong); border-radius: inherit; }
.count {
  position: absolute;
  right: -6px;
  bottom: -6px;
  min-width: 24px;
  padding: 3px 6px;
  border-radius: 999px;
  background: var(--ink-0);
  border: 1px solid var(--line-strong);
  color: var(--text);
  font-family: var(--font-display);
  font-size: 0.85rem;
  font-weight: 700;
  line-height: 1;
  text-align: center;
}
.label { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: var(--text-faint); white-space: nowrap; }

.pile-dialog {
  width: min(60rem, 100vw - 2rem);
  max-height: min(44rem, 100dvh - 2rem);
  padding: 0;
  border: 1px solid var(--line-strong);
  border-radius: 14px;
  background: var(--ink-2);
  color: var(--text);
  box-shadow: 0 20px 60px var(--shadow-strong);
}
.pile-dialog::backdrop { background: rgb(0 0 0 / 0.6); backdrop-filter: blur(3px); }
.pile-dialog header { display: flex; align-items: center; justify-content: space-between; padding: 12px 16px; border-bottom: 1px solid var(--line); }
.pile-dialog h2 { margin: 0; font-size: 1.4rem; text-transform: uppercase; }
.pile-dialog h2 small { color: var(--text-faint); font-size: 0.9rem; margin-left: 6px; }
.pile-dialog .cards { display: flex; flex-wrap: wrap; gap: 14px; padding: 16px; overflow-y: auto; --card-width: 120px; }
</style>
