<script setup lang="ts">
// What is being paid for, held up while it is: the card played, or the card whose
// ability it is, with the resources paid so far and the cards spent on them.
import { computed } from 'vue'
import GameIcon from '../browser/components/GameIcon.vue'
import type { ActiveCostView } from '../server'
import BoardCard from './BoardCard.vue'

const { cost, things = {}, player = null } = defineProps<{
  cost: ActiveCostView
  things?: Record<string, { code: string; name: string }>
  player?: string | null
}>()
const paying = computed(() => cost.card ?? (cost.source && things[cost.source] ? { id: cost.source, ...things[cost.source] } : null))
</script>

<template>
  <section class="paying" :aria-label="$t('game.paying.title')">
    <header>
      <span class="eyebrow">{{ player ? $t('game.paying.by', { name: player }) : $t('game.paying.title') }}</span>
      <h2>{{ paying?.name ?? $t('game.paying.title') }}</h2>
    </header>
    <div class="body">
      <BoardCard v-if="paying" :id="paying.id" :code="paying.code" :name="paying.name" size="sm" :morph="false" />
      <div class="ledger">
        <span class="label">{{ $t('game.paying.paid') }}</span>
        <p v-if="cost.paid.length" class="resources">
          <GameIcon v-for="(r, i) in cost.paid" :key="i" :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
        </p>
        <p v-else class="none">{{ $t('game.paying.nothingYet') }}</p>
        <template v-if="cost.spent.length">
          <span class="label">{{ $t('game.paying.spent') }}</span>
          <div class="spent">
            <BoardCard v-for="card in cost.spent" :id="card.id" :key="card.id" :code="card.code" :name="card.name" size="xs" :morph="false" />
          </div>
        </template>
      </div>
    </div>
  </section>
</template>

<style scoped>
.paying {
  --card-width: 92px;
  display: grid;
  gap: 10px;
  padding: 14px;
  border-radius: 12px;
  background: linear-gradient(160deg, rgb(226 163 59 / 0.12), transparent 60%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.4);
  animation: rise 0.25s ease-out;
}
@keyframes rise { from { opacity: 0; transform: translateY(6px); } }
header { display: grid; gap: 2px; }
.eyebrow { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--amber); }
h2 { margin: 0; font-size: 1.2rem; line-height: 1.05; text-transform: uppercase; }
.body { display: flex; gap: 12px; align-items: flex-start; }
.ledger { display: flex; flex-direction: column; gap: 6px; min-width: 0; }
.label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--text-faint); }
.resources { display: flex; flex-wrap: wrap; gap: 4px; margin: 0; font-size: 1.5rem; }
.resources :deep(.game-icon) { padding: 3px; border-radius: 8px; background: var(--ink-0); border: 1px solid var(--line); width: 1.25em; height: 1.25em; }
.none { margin: 0; font-size: 0.8rem; font-style: italic; color: var(--text-faint); }
.spent { display: flex; flex-wrap: wrap; gap: 6px; }
</style>
