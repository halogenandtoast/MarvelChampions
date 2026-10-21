<script setup lang="ts">
// What is being paid for, held up while it is: the card played, or the card whose
// ability it is, with the resources paid so far and the cards spent on them. A lane of the
// river.
import { computed } from 'vue'
import GameIcon from '../browser/components/GameIcon.vue'
import type { ActiveCostView } from '../server'
import BoardCard from './BoardCard.vue'

const { cost, things = {}, player = null, elsewhere } = defineProps<{
  cost: ActiveCostView
  things?: Record<string, { code: string; name: string }>
  player?: string | null
  /** ids drawn elsewhere on the board too, which must not share a morph */
  elsewhere: ReadonlySet<string>
}>()
const covered = computed(() => !cost.anyAmount && !(cost.owed ?? []).length)
const paying = computed(() => cost.card ?? (cost.source && things[cost.source] ? { id: cost.source, ...things[cost.source] } : null))
</script>

<template>
  <section class="paying" :class="{ covered }" :aria-label="$t('game.paying.title')">
    <header>
      <span class="eyebrow">{{ player ? $t('game.paying.by', { name: player }) : $t('game.paying.title') }}</span>
      <h2>{{ paying?.name ?? $t('game.paying.title') }}</h2>
      <span class="label">{{ $t('game.paying.paid') }}</span>
      <p v-if="cost.paid.length" class="resources">
        <GameIcon v-for="(r, i) in cost.paid" :key="i" :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
      </p>
      <p v-else class="none">{{ $t('game.paying.nothingYet') }}</p>
      <p v-if="cost.anyAmount" class="owed">{{ $t('game.paying.anyAmount') }}</p>
      <p v-else-if="covered" class="owed done">{{ $t('game.paying.covered') }}</p>
      <p v-else class="owed">
        {{ $t('game.paying.needs', { count: cost.owed.length }) }}
        <template v-for="(r, i) in cost.owed" :key="i">
          <GameIcon v-if="r" class="due" :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
          <span v-else class="due any" :title="$t('game.paying.anyResource')" />
        </template>
      </p>
    </header>
    <BoardCard v-if="paying" :id="paying.id" :code="paying.code" :name="paying.name" :morph="!elsewhere.has(paying.id)" />
    <div v-if="cost.spent.length" class="spent">
      <span class="label">{{ $t('game.paying.spent') }}</span>
      <div class="spent-cards">
        <BoardCard v-for="card in cost.spent" :id="card.id" :key="card.id" :code="card.code" :name="card.name" :morph="!elsewhere.has(card.id)" />
      </div>
    </div>
  </section>
</template>

<style scoped>
.paying {
  --spent-width: calc(var(--card-width) * 0.72);
  display: flex;
  gap: 14px;
  align-items: center;
  flex: 1 1 0;
  min-width: 0;
  padding: 12px 16px;
  border-radius: 14px;
  background: linear-gradient(160deg, rgb(226 163 59 / 0.12), transparent 60%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.4);
  animation: rise 0.25s ease-out;
}
@keyframes rise { from { opacity: 0; transform: translateY(6px); } }
header { display: flex; flex-direction: column; gap: 4px; min-width: 0; flex: 0 1 auto; }
.eyebrow { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--amber); }
h2 { margin: 0 0 4px; font-size: 1.2rem; line-height: 1.05; text-transform: uppercase; }
.label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--text-faint); }
.resources { display: flex; flex-wrap: wrap; gap: 4px; margin: 0; font-size: 1.5rem; }
.resources :deep(.game-icon) { padding: 3px; border-radius: 8px; background: var(--ink-0); border: 1px solid var(--line); width: 1.25em; height: 1.25em; }
.paying.covered { border-color: rgb(79 174 106 / 0.6); background: linear-gradient(160deg, rgb(79 174 106 / 0.14), transparent 60%), var(--ink-2); }
.owed { display: flex; flex-wrap: wrap; align-items: center; gap: 4px; margin: 4px 0 0; font-size: 0.75rem; font-weight: 600; color: var(--amber); }
.owed.done { color: var(--health); }
.owed.done::before { content: '✓'; font-weight: 800; }
.due { width: 1.1rem; height: 1.1rem; opacity: 0.85; }
.due.any { display: inline-block; border-radius: 50%; border: 2px dashed var(--amber); box-sizing: border-box; }
.none { margin: 0; font-size: 0.8rem; font-style: italic; color: var(--text-faint); }
/* spent cards smaller, under their label, to stay within the river's height */
.spent { display: flex; flex-direction: column; gap: 4px; min-width: 0; --card-width: var(--spent-width); }
.spent-cards { display: flex; gap: 6px; overflow-x: auto; }
</style>
