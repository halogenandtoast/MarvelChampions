<script setup lang="ts">
// The villain's side of the table: the villain, the main scheme with its threat, the side
// schemes, and the encounter deck and discard.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import type { GameView, SchemeView } from '../server'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import Pile from './Pile.vue'
import TokenStack from './TokenStack.vue'

const { game } = defineProps<{ game: GameView }>()
const { t } = useI18n()
const cards = useCards()

const villains = computed(() => game.villains ?? [])
const mainSchemes = computed(() => game.mainSchemes ?? [])
const sideSchemes = computed(() => game.sideSchemes ?? [])
const environments = computed(() => game.environments ?? [])
const heroes = computed(() => Math.max(1, (game.players ?? []).length))

// the threat that completes a scheme, printed per hero unless fixed
const target = (scheme: SchemeView) => {
  const card = cards.card(scheme.code)
  if (typeof card?.threat !== 'number') return null
  return card.threatFixed ? card.threat : card.threat * heroes.value
}
const escalation = (scheme: SchemeView) => {
  const card = cards.card(scheme.code)
  if (typeof card?.escalationThreat !== 'number') return null
  return card.escalationThreatFixed ? card.escalationThreat : card.escalationThreat * heroes.value
}
</script>

<template>
  <section class="villain-area" :aria-label="t('game.zones.villain')">
    <div v-for="villain in villains" :key="villain.id" class="villain">
      <div class="body">
        <dl class="stats">
          <div class="stat-sch"><dt>{{ t('game.stats.sch') }}</dt><dd>{{ villain.scheme }}</dd></div>
          <div class="stat-atk"><dt>{{ t('game.stats.atk') }}</dt><dd>{{ villain.attack }}</dd></div>
        </dl>
        <BoardCard
          :id="villain.id"
          :code="villain.code"
          :name="villain.name"
          :damage="villain.damage"
          :stunned="villain.stunned"
          :confused="villain.confused"
          :tough="villain.tough"
        />
      </div>
      <Meter :label="t('game.hp')" :value="Math.max(0, villain.hitPoints - villain.damage)" :max="villain.hitPoints" tone="villain" />
    </div>
    <!-- what is attached to the villain, as cards beside it -->
    <template v-for="villain in villains" :key="`${villain.id}-attached`">
      <BoardCard v-for="a in villain.attachments ?? []" :id="a.id" :key="a.id" class="villain-attached" :code="a.code" :name="a.name" :damage="a.damage ?? 0" :counters="a.counters || null" />
      <BoardCard v-for="u in villain.upgrades ?? []" :id="u.id" :key="u.id" class="villain-attached" :code="u.code" :name="u.name" :exhausted="u.exhausted" :counters="u.uses ?? null" />
    </template>

    <!-- the schemes, to the right beside the encounter deck -->
    <div class="schemes">
      <div v-for="scheme in mainSchemes" :key="scheme.id" class="scheme main">
        <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.threat" :flag="scheme.crisis ? t('game.crisis') : null" flag-icon="crisis" :counters="scheme.counters || null" />
        <Meter :label="t('game.threat')" :value="scheme.threat" :max="target(scheme)" tone="threat" />
        <p v-if="escalation(scheme) !== null" class="escalation">
          {{ t('game.perRound', { count: escalation(scheme)! + (game.accelerationTokens ?? 0) }) }}
          <TokenStack
            v-if="game.accelerationTokens"
            kind="acceleration"
            :count="game.accelerationTokens"
            :title="t('game.acceleration', { count: game.accelerationTokens })"
          />
        </p>
      </div>

      <div v-if="sideSchemes.length" class="side-schemes">
        <div v-for="scheme in sideSchemes" :key="scheme.id" class="scheme side">
          <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.threat" :flag="scheme.crisis ? t('game.crisis') : null" flag-icon="crisis" :attached="scheme.upgrades ?? []" />
        </div>
      </div>

      <div v-if="environments.length" class="side-schemes environments">
        <div v-for="environment in environments" :key="environment.id" class="scheme side">
          <BoardCard :id="environment.id" :code="environment.code" :name="environment.name" :attached="environment.attachments ?? []" :counters="environment.counters || null" />
        </div>
      </div>
    </div>

    <div class="encounter-piles">
      <Pile :label="t('game.piles.encounterDeck')" back="encounter" :count="game.encounterDeckSize ?? 0" pile="deck-encounter" />
      <Pile :label="t('game.piles.encounterDiscard')" :cards="game.encounterDiscard ?? []" pile="discard-encounter" />
    </div>
  </section>
</template>

<style scoped>
.villain-area {
  position: relative;
  display: flex;
  align-items: flex-start;
  gap: 18px;
  padding: 14px 16px;
  border-radius: 14px;
  background:
    linear-gradient(100deg, rgb(184 50 58 / 0.13), transparent 45%),
    linear-gradient(var(--ink-2), var(--ink-2));
  border: 1px solid rgb(184 50 58 / 0.22);
  box-shadow: inset 0 1px 0 rgb(255 255 255 / 0.04);
  overflow-x: auto;
  --attached-width: calc(var(--card-width) * 0.8);
}
/* what is attached to the villain a little smaller than the villain */
.villain-attached { --card-width: var(--attached-width); }
.villain { display: flex; flex-direction: column; gap: 8px; flex: none; }
/* its stats pinned to the card's left */
.body { display: flex; gap: 6px; align-items: flex-start; }
/* its health under the card alone */
.villain > :deep(.meter) { width: var(--card-width); align-self: flex-end; }
.stats { display: flex; flex-direction: column; gap: 4px; margin: 0; width: 44px; flex: none; }
.stats div { flex: 1; padding: 5px 0 4px; border-radius: 7px; background: var(--stat-bg, var(--ink-0)); border: 1px solid var(--stat-bg, var(--line)); text-align: center; }
.stats dt { font-size: 0.5rem; font-weight: 700; letter-spacing: 0.12em; color: var(--stat-label, var(--text-faint)); }
.stats dd { margin: 0; font-family: var(--font-display); font-size: 1.25rem; font-weight: 800; line-height: 1.05; color: var(--stat-fg, inherit); }

.scheme { display: flex; flex-direction: column; gap: 6px; flex: none; }
.scheme.main :deep(.meter) { width: calc(var(--card-width) / var(--card-aspect)); }
.escalation { display: flex; align-items: center; gap: 6px; margin: 0; font-size: 0.7rem; color: var(--text-faint); --token-size: 20px; }
.side-schemes { display: flex; flex-wrap: wrap; gap: 10px; align-content: flex-start; flex: 0 1 auto; min-width: 0; }

.schemes { display: flex; gap: 18px; align-items: flex-start; margin-left: auto; min-width: 0; }
.encounter-piles { display: flex; gap: 14px; flex: none; padding-top: 4px; }
</style>
