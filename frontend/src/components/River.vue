<script setup lang="ts">
// Between the villain's side and the players': what is happening now. An encounter card
// as it is revealed, an attack with its boost cards dealt face down and turned up, a card
// being paid for (the default slot). Each is a lane, splitting the river between them; it
// keeps its height when empty so the board never jumps.
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import BoardCard from './BoardCard.vue'

export interface RiverCard {
  id: string
  /** null while face down */
  code: string | null
  name?: string | null
}

const { cards, label = null, elsewhere, shared = false } = defineProps<{
  cards: readonly RiverCard[]
  label?: string | null
  /** another lane is in the slot */
  shared?: boolean
  /** ids drawn elsewhere on the board too, which must not share a morph */
  elsewhere: ReadonlySet<string>
}>()
const { t } = useI18n()
const names = useCards()
</script>

<template>
  <section class="river" :aria-label="t('game.zones.river')">
    <div v-if="cards.length || label || !shared" class="lane" :class="{ live: cards.length || label }">
      <p class="what">{{ label ?? (cards.length ? t('game.river.revealed') : '') }}</p>
      <div class="cards">
        <BoardCard
          v-for="card in cards"
          :id="card.id"
          :key="card.id"
          :code="card.code ?? ''"
          :name="card.name || names.name(card.code)"
          :back="card.code ? null : 'encounter'"
          :morph="!elsewhere.has(card.id)"
        />
      </div>
    </div>
    <slot />
  </section>
</template>

<style scoped>
.river { display: flex; gap: 12px; min-height: 0; min-width: 0; }
.lane {
  display: grid;
  grid-template-columns: minmax(110px, 0.3fr) minmax(0, 1fr);
  align-items: center;
  gap: 16px;
  flex: 1 1 0;
  min-width: 0;
  padding: 12px 16px;
  border-radius: 14px;
  background: var(--ink-1);
  border: 1px dashed var(--line);
  transition: background 0.3s ease, border-color 0.3s ease;
}
.lane.live {
  background: radial-gradient(ellipse at 50% 50%, rgb(226 163 59 / 0.12), transparent 70%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.35);
}
.what {
  margin: 0;
  font-family: var(--font-display);
  font-size: 1.15rem;
  font-weight: 700;
  line-height: 1.1;
  text-transform: uppercase;
  color: var(--amber);
}
.cards { display: flex; gap: 12px; align-items: center; justify-content: center; min-width: 0; overflow-x: auto; }
</style>
