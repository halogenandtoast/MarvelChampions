<script setup lang="ts">
import { assetUrl } from '../assets'
// Between the villain's side and the players': what is happening now. An encounter card
// as it is revealed, an attack with its attacker as it stands and its boost cards dealt
// face down and turned up, a card
// being paid for (the default slot). Each is a lane, splitting the river between them; it
// keeps its height when empty so the board never jumps.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import TokenStack from './TokenStack.vue'

export interface RiverCard {
  id: string
  /** null while face down */
  code: string | null
  name?: string | null
}

/** a scheme threat would be placed on, as it stands and as it would be */
export interface RiverScheme {
  id: string
  code: string
  name: string
  from: number
  amount: number
  /** the threat that advances a main scheme */
  threshold: number | null
}

export interface RiverAttacker {
  id: string
  code: string
  name: string
  attack: number | null
  boostCards?: number
}

const { cards, label = null, reaction = false, attacker = null, scheme = null, canceledEvent = null, elsewhere, shared = false } = defineProps<{
  canceledEvent?: { code: string; statuses: readonly ('stunned' | 'confused')[] } | null
  cards: readonly RiverCard[]
  label?: string | null
  /** an interrupt or response window, highlighted like its available cards */
  reaction?: boolean
  /** the enemy attacking, with its attack as modified now */
  attacker?: RiverAttacker | null
  /** the scheme threat would be placed on */
  scheme?: RiverScheme | null
  /** another lane is in the slot */
  shared?: boolean
  /** ids drawn elsewhere on the board too, which must not share a morph */
  elsewhere: ReadonlySet<string>
}>()
const { t } = useI18n()
const names = useCards()
const advances = computed(() => !!scheme && scheme.threshold !== null && scheme.from + scheme.amount >= scheme.threshold)
</script>

<template>
  <section class="river" :aria-label="t('game.zones.river')">
    <div v-if="cards.length || label || canceledEvent || !shared" class="lane" :class="{ live: cards.length || label || canceledEvent, reaction, danger: advances }">
      <div class="what">
        <p>{{ label ?? (cards.length ? t('game.river.revealed') : '') }}</p>
        <p v-for="status in canceledEvent?.statuses ?? []" :key="status">{{ t(`game.river.${status}`) }}</p>
        <p v-if="advances" class="warn" role="alert"><span aria-hidden="true">▲</span> {{ t('game.river.advances') }}</p>
      </div>
      <div class="cards">
        <template v-if="canceledEvent">
          <BoardCard :code="canceledEvent.code" :morph="false" />
          <img v-for="status in canceledEvent.statuses" :key="status" class="canceled-status" :src="assetUrl(`/images/tokens/${status}.avif`)" :alt="t(`game.status.${status}`)" />
        </template>
        <div v-if="attacker" class="attacker">
          <dl class="stats">
            <div class="stat-atk"><dt>{{ t('game.stats.atk') }}</dt><dd>{{ attacker.attack ?? '–' }}</dd></div>
            <div v-if="attacker.boostCards"><dt>{{ t('game.stats.boost') }}</dt><dd>{{ attacker.boostCards }}</dd></div>
          </dl>
          <BoardCard :id="attacker.id" :code="attacker.code" :name="attacker.name" :morph="false" />
        </div>
        <div v-if="scheme" class="scheme">
          <div class="scheme-card">
            <TokenStack class="adding" kind="threat" :count="scheme.amount" :title="t('game.threatOn', { count: scheme.amount })" />
            <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.from" :morph="false" />
          </div>
          <Meter :label="t('game.threat')" :value="scheme.from" :max="scheme.threshold" :adding="scheme.amount" tone="threat" />
        </div>
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
.canceled-status { width: var(--card-width); height: auto; border-radius: 8px; }
.lane {
  display: grid;
  grid-template-columns: minmax(110px, 0.3fr) minmax(0, 1fr);
  align-items: center;
  gap: 16px;
  flex: 1 1 0;
  min-width: 0;
  padding: 6px 12px;
  border-radius: 14px;
  background: var(--ink-1);
  border: 1px dashed var(--line);
  transition: background 0.3s ease, border-color 0.3s ease;
}
.lane.live {
  background: radial-gradient(ellipse at 50% 50%, rgb(226 163 59 / 0.12), transparent 70%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.35);
}
.what { display: flex; flex-direction: column; gap: 6px; }
.lane.reaction {
  background: color-mix(in srgb, var(--interactive) 8%, var(--ink-2));
  border-color: var(--interactive);
  box-shadow: 0 0 16px var(--interactive-glow), inset 0 0 16px var(--interactive-glow);
}
.lane.reaction .what { align-self: center; justify-self: start; }
.lane.reaction .what p { color: var(--interactive); }
.what p {
  margin: 0;
  font-family: var(--font-display);
  font-size: 1.15rem;
  font-weight: 700;
  line-height: 1.1;
  text-transform: uppercase;
  color: var(--amber);
}
/* threat that would advance the scheme */
.lane.danger {
  background: radial-gradient(ellipse at 50% 50%, rgb(220 50 50 / 0.2), transparent 70%), var(--ink-2);
  border-color: var(--crimson-bright);
  animation: danger 1.4s ease-in-out infinite alternate;
}
.lane.danger .what p:not(.warn) { color: var(--crimson-bright); }
@keyframes danger {
  from { box-shadow: 0 0 6px rgb(220 50 50 / 0.35); }
  to { box-shadow: 0 0 22px rgb(220 50 50 / 0.75); }
}
@media (prefers-reduced-motion: reduce) { .lane.danger { animation: none; box-shadow: 0 0 16px rgb(220 50 50 / 0.6); } }
/* the threshold reached: said, not only coloured */
.what .warn {
  align-self: flex-start;
  padding: 3px 8px;
  border-radius: 999px;
  background: var(--crimson-bright);
  color: white;
  font-size: 0.75rem;
  letter-spacing: 0.06em;
}
/* the meter under the card, as on the board */
.scheme { display: flex; flex-direction: column; align-items: flex-end; gap: 6px; flex: none; }
.scheme-card { display: flex; align-items: center; gap: 8px; }
/* the threat about to go on, to the card's left */
.adding { --token-size: 30px; }
.scheme :deep(.meter) { width: calc(var(--card-width) / var(--card-aspect)); }
/* stats to the card's left, not under it, so the river stays a card high */
.attacker { display: flex; gap: 6px; align-items: flex-start; flex: none; }
.stats { display: flex; flex-direction: column; gap: 4px; margin: 0; min-width: 44px; }
.stats div { padding: 4px 0 3px; border-radius: 7px; background: var(--stat-bg, var(--ink-0)); border: 1px solid var(--stat-bg, var(--line)); text-align: center; }
.stats dt { font-size: 0.5rem; font-weight: 700; letter-spacing: 0.12em; color: var(--stat-label, var(--text-faint)); }
.stats dd { margin: 0; font-family: var(--font-display); font-size: 1.15rem; font-weight: 800; line-height: 1.05; color: var(--stat-fg, inherit); }
/* ponytail: no scroll box, so a lit card's ring and glow show; a scroll box crops them. Add one back if a lane ever holds more cards than fit */
.cards { display: flex; gap: 12px; align-items: center; min-width: 0; }
</style>
