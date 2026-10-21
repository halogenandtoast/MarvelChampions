<script setup lang="ts">
// A player's hand, fanned along the foot of the board; someone else's hand at a game
// with friends is its backs.
import { computed } from 'vue'
import type { CardView } from '../server'
import BoardCard from './BoardCard.vue'

const { cards = null, count = 0, label } = defineProps<{ cards?: readonly CardView[] | null; count?: number; label: string }>()
const shown = computed(() => cards ?? Array.from({ length: count }, (_, i) => ({ id: `hidden-${i}`, code: '', name: '' })))
const spread = computed(() => Math.min(5, 26 / Math.max(1, shown.value.length)))
const place = (i: number) => {
  const offset = i - (shown.value.length - 1) / 2
  return { '--rotate': `${offset * spread.value}deg`, '--drop': `${Math.abs(offset) ** 1.6 * 2.2}px`, zIndex: i }
}
</script>

<template>
  <section class="hand" :aria-label="label">
    <span class="hand-label">{{ label }} <b>{{ shown.length }}</b></span>
    <div class="fan">
      <div v-for="(card, i) in shown" :key="card.id" class="in-hand" :style="place(i)">
        <BoardCard v-if="cards" :id="card.id" :code="card.code" :name="card.name" />
        <BoardCard v-else back="player" />
      </div>
      <p v-if="!shown.length" class="empty">{{ $t('game.emptyHand') }}</p>
    </div>
  </section>
</template>

<style scoped>
.hand { position: relative; display: flex; flex-direction: column; align-items: center; gap: 2px; padding-top: 6px; }
.hand-label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--text-faint); }
.hand-label b { color: var(--text-muted); margin-left: 4px; }
.fan { display: flex; justify-content: center; align-items: flex-end; padding: 14px 24px 6px; min-height: calc(var(--card-width) / var(--card-aspect) + 20px); }
.in-hand {
  position: relative;
  margin-inline: calc(var(--card-width) * -0.1);
  transform: translateY(var(--drop)) rotate(var(--rotate));
  transform-origin: 50% 120%;
  transition: transform 0.18s ease, margin 0.18s ease;
}
.in-hand:hover, .in-hand:focus-within {
  z-index: 50 !important;
  transform: translateY(-14px) rotate(0deg) scale(1.06);
}
.in-hand:has(:deep(.card.active)) { transform: translateY(calc(var(--drop) - 8px)) rotate(var(--rotate)); }
.in-hand:has(:deep(.card.active)):hover { transform: translateY(-16px) rotate(0deg) scale(1.06); }
.fan:has(.empty) { min-height: 0; padding: 4px; }
.empty { margin: auto; color: var(--text-faint); font-size: 0.8rem; font-style: italic; }
</style>
