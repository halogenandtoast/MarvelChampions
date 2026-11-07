<script setup lang="ts">
import { assetUrl } from '../assets'
// Between the villain's side and the players': what is happening now. An encounter card
// as it is revealed, an attack with its attacker as it stands and its boost cards dealt
// face down and turned up, a card
// being paid for (the default slot). Each is a lane, splitting the river between them; it
// keeps its height when empty so the board never jumps.
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import TokenStack from './TokenStack.vue'
import { useChoiceLabel } from '../composables/useChoices'
import type { ChoiceView } from '../server'

export interface RiverCard {
  id: string
  boost?: boolean
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
export interface RiverSchemer {
  id: string
  code: string
  name: string
  scheme: number | null
  boostCards?: number
}

const { cards, revealedCards = [], resolvingEvents = [], label = null, reactionCards = [], reactionLabel = null, activationAlert = null, activationBanner = false, activationKind = 'attack', actions = [], attacker = null, schemer = null, scheme = null, canceledEvent = null, elsewhere, shared = false } = defineProps<{
  activationKind?: 'attack' | 'scheme' | 'playerPhase' | 'villainPhase'
  schemer?: RiverSchemer | null
  activationBanner?: boolean
  actions?: ChoiceView[]
  activationAlert?: string | null
  canceledEvent?: { code: string; statuses: readonly ('stunned' | 'confused')[] } | null
  cards: readonly RiverCard[]
  resolvingEvents?: readonly RiverCard[]
  revealedCards?: readonly RiverCard[]
  label?: string | null
  /** an interrupt or response window, highlighted like its available cards */
  reactionCards?: readonly RiverCard[]
  reactionLabel?: string | null
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
const emit = defineEmits<{ choose: [index: number] }>()
const choiceLabel = useChoiceLabel()
const names = useCards()
const skipAnimation = ref(false)
const cancelAnimation = () => { skipAnimation.value = true }
onMounted(() => window.addEventListener('marvel:cancel-animations', cancelAnimation))
onBeforeUnmount(() => window.removeEventListener('marvel:cancel-animations', cancelAnimation))
watch(() => activationAlert, alert => { if (alert) skipAnimation.value = false })
const enemy = computed(() => attacker ?? schemer)
const enemyPower = computed(() => attacker ? attacker.attack : schemer?.scheme)
const displayedCards = computed(() => cards.filter(card => card.id !== enemy.value?.id && !reactionCards.some(reaction => reaction.id === card.id)))
const advances = computed(() => !!scheme && scheme.threshold !== null && scheme.from + scheme.amount >= scheme.threshold)
const hasMainLane = computed(() => !!(cards.length || label || enemy.value || scheme || canceledEvent || (!shared && !reactionLabel && !revealedCards.length && !resolvingEvents.length)))
</script>

<template>
  <section class="river" :aria-label="t('game.zones.river')">
    <Transition name="activation" :css="!skipAnimation">
      <div v-if="activationAlert" class="activation-alert" :class="{ 'fight-banner': activationBanner, 'scheme-banner': activationKind === 'scheme' || activationKind === 'playerPhase' }" role="status"><span>{{ activationAlert }}</span></div>
    </Transition>
    <section v-if="reactionLabel" class="lane live reaction" :class="{ compact: hasMainLane || shared || revealedCards.length || resolvingEvents.length }" :aria-label="reactionLabel">
      <div class="what"><p>{{ reactionLabel }}</p></div>
      <div class="cards">
        <BoardCard v-for="card in reactionCards" :id="card.id" :key="card.id" :code="card.code ?? ''" :name="card.name || names.name(card.code)" :morph="false" />
      </div>
    </section>
    <section v-if="resolvingEvents.length" class="lane live resolving" :class="{ compact: hasMainLane || shared || reactionLabel || revealedCards.length }" :aria-label="t('game.river.resolving')">
      <div class="what"><p>{{ t('game.river.resolving') }}</p></div>
      <div class="cards">
        <BoardCard v-for="card in resolvingEvents" :id="card.id" :key="card.id" :code="card.code ?? ''" :name="card.name || names.name(card.code)" :morph="!elsewhere.has(card.id)" />
      </div>
    </section>
    <section v-if="revealedCards.length" class="lane live revealed" :class="{ compact: hasMainLane || shared || reactionLabel || resolvingEvents.length }" :aria-label="t('game.river.revealed')">
      <div class="what"><p>{{ t('game.river.revealed') }}</p></div>
      <div class="cards">
        <BoardCard
          v-for="card in revealedCards"
          :id="card.id"
          :key="card.id"
          :code="card.code ?? ''"
          :name="card.name || names.name(card.code)"
          :back="card.code ? null : 'encounter'"
          flip-reveal
          :morph="!elsewhere.has(card.id)"
        />
      </div>
    </section>
    <div v-if="hasMainLane" class="lane" :class="{ live: cards.length || label || enemy || canceledEvent, 'attack-lane': attacker, 'scheme-lane': !attacker && (schemer || scheme), danger: advances }">
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
        <div v-if="enemy" class="attacker">
          <dl class="stats">
            <div :class="attacker ? 'stat-atk' : 'stat-sch'"><dt>{{ t(attacker ? 'game.stats.atk' : 'game.stats.sch') }}</dt><dd>{{ enemyPower ?? '–' }}</dd></div>
            <div v-if="enemy.boostCards"><dt>{{ t('game.stats.boost') }}</dt><dd>{{ enemy.boostCards }}</dd></div>
          </dl>
          <div class="enemy-card">
            <BoardCard :id="enemy.id" :code="enemy.code" :name="enemy.name" :morph="false" />
            <div v-if="actions.length" class="river-actions">
              <button v-for="action in actions" :key="action.index" type="button" class="btn primary" @click="emit('choose', action.index)">{{ choiceLabel(action) }}</button>
            </div>
          </div>
        </div>
        <div v-if="scheme" class="scheme">
          <div class="scheme-card">
            <TokenStack class="adding" kind="threat" :count="scheme.amount" :title="t('game.threatOn', { count: scheme.amount })" />
            <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.from" :morph="false" />
          </div>
          <Meter :label="t('game.threat')" :value="scheme.from" :max="scheme.threshold" :adding="scheme.amount" tone="threat" />
        </div>
        <BoardCard
          v-for="card in displayedCards"
          :id="card.id"
          :key="card.id"
          :code="card.code ?? ''"
          :name="card.name || names.name(card.code)"
          :back="card.code ? null : 'encounter'"
          flip-reveal
          :morph="!elsewhere.has(card.id)"
        />
      </div>
      <div v-if="actions.length && !enemy" class="river-actions">
        <button v-for="action in actions" :key="action.index" type="button" class="btn primary" @click="emit('choose', action.index)">{{ choiceLabel(action) }}</button>
      </div>
    </div>
    <slot />
  </section>
</template>

<style scoped>
.river { position: relative; display: flex; flex-wrap: wrap; gap: 12px; min-height: 0; min-width: 0; }
.activation-alert { position: absolute; z-index: 5; inset: 0; display: grid; place-items: center; padding: 16px; border: 1px solid var(--amber); border-radius: 14px; background: var(--ink-2); color: var(--amber); font-family: var(--font-display); font-size: clamp(1.1rem, 2vw, 1.7rem); font-weight: 800; text-align: center; text-transform: uppercase; pointer-events: none; }
.activation-enter-active, .activation-leave-active { transition: opacity 180ms ease, transform 180ms ease; }
.activation-enter-from { opacity: 0; transform: translateY(6px); }
.activation-leave-to { opacity: 0; transform: translateY(-4px); }
.fight-banner { overflow: hidden; border: 0; background: var(--banner-color, var(--stat-atk)); color: var(--ink-1); font-size: clamp(1.6rem, 3.5vw, 3.5rem); font-style: italic; letter-spacing: -0.025em; animation: fight-flood 2200ms both; }
.scheme-banner { --banner-color: var(--stat-sch); }
.fight-banner span { position: relative; z-index: 1; animation: fight-title 2200ms both; }
.fight-banner::after { content: ''; position: absolute; inset: -50% -20%; background: repeating-linear-gradient(-55deg, transparent 0 50px, rgb(0 0 0 / 0.08) 50px 65px); animation: fight-streak 6600ms linear both; }
.fight-banner.activation-enter-active, .fight-banner.activation-leave-active { transition: none; }
@keyframes fight-flood { 0% { clip-path: inset(0 100% 0 0); transform: scaleY(0.25); } 18%, 76% { clip-path: inset(0); transform: scaleY(1); opacity: 1; } 100% { clip-path: inset(0); transform: scale(0.84); opacity: 0; } }
@keyframes fight-title { 0% { transform: translateX(-12%) scale(1.6); opacity: 0; } 40% { transform: translateX(1%) scale(1.05); opacity: 1; } 52%, 76% { transform: translateX(0) scale(1); opacity: 1; } 100% { transform: scale(0.8); opacity: 0; } }
@keyframes fight-streak { from { transform: translateX(-12%); } to { transform: translateX(12%); } }
@media (prefers-reduced-motion: reduce) { .fight-banner, .fight-banner span, .fight-banner::after { animation: none; } }
@media (prefers-reduced-motion: reduce) { .activation-enter-active, .activation-leave-active { transition: none; } .activation-enter-from, .activation-leave-to { transform: none; } }
.canceled-status { width: var(--card-width); height: auto; border-radius: 8px; }
.lane {
  display: grid;
  grid-template-columns: minmax(110px, 0.3fr) minmax(0, 1fr);
  align-items: center;
  gap: 16px;
  flex: 1 1 360px;
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
.lane.compact { flex: 0 1 auto; grid-template-columns: auto auto; }
.lane.compact .what { max-width: 110px; }
.what p {
  margin: 0;
  font-family: var(--font-display);
  font-size: 1.15rem;
  font-weight: 700;
  line-height: 1.1;
  text-transform: uppercase;
  color: var(--amber);
}
.lane.attack-lane { --activation-color: var(--stat-atk); }
.lane.scheme-lane { --activation-color: var(--stat-sch); }
.lane.attack-lane, .lane.scheme-lane {
  background: color-mix(in oklab, var(--activation-color) 10%, var(--ink-2));
  border-color: color-mix(in oklab, var(--activation-color) 45%, var(--ink-2));
}
.lane.attack-lane .what p, .lane.scheme-lane .what p { color: color-mix(in oklab, var(--activation-color) 75%, white); }
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
.cards { display: flex; gap: 12px; align-items: flex-start; min-width: 0; }
.river-actions { grid-column: 1 / -1; display: flex; justify-content: flex-end; }
.enemy-card { display: flex; flex-direction: column; gap: 10px; width: var(--card-width); }
.enemy-card .river-actions { justify-content: stretch; }
.enemy-card .river-actions button { width: 100%; padding: 8px 6px; font-size: 0.8rem; }
/* Keep a boost's frame identical while the back flips to its printed face. */
.cards :deep(.card:not(.landscape):not(.unfolded)) { height: calc(var(--card-width) / var(--card-aspect)); }
</style>
