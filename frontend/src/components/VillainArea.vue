<script setup lang="ts">
// The villain's side of the table: the villain, the main scheme with its threat, the side
// schemes, and the encounter deck and discard. With several game areas, only what is in the
// area given; with several villains, the active one marked; a villain's own deck, and any
// other deck, as piles of their own.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import type { AreaView, DeckView, GameView, SchemeView } from '../server'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import Pile from './Pile.vue'
import GameIcon from '../browser/components/GameIcon.vue'
import TokenStack from './TokenStack.vue'

const { game, area = null } = defineProps<{ game: GameView; area?: AreaView | null }>()
const { t, te } = useI18n()
const cards = useCards()

// what is in the area, in the area's order, or everything when there is no area to narrow to
const inArea = <T extends { id: string }>(xs: T[] | undefined, ids: string[] | undefined): T[] =>
  area && ids ? ids.flatMap((id) => (xs ?? []).filter((x) => x.id === id)) : (xs ?? [])
// the villains as they were set out (Wrecker, Thunderball, Piledriver, Bulldozer)
const villains = computed(() => {
  const order = area?.villains ?? game.areas?.[0]?.villains
  return order ? order.flatMap((id) => (game.villains ?? []).filter((v) => v.id === id)) : (game.villains ?? [])
})
const mainSchemes = computed(() => inArea(game.mainSchemes, area?.mainSchemes))
const sideSchemes = computed(() => inArea(game.sideSchemes, area?.sideSchemes))
// a Spell in a player's play area is drawn there
const environments = computed(() => inArea(game.environments?.filter((e) => !e.owner), area?.environments))
const decks = computed(() => area?.decks ?? game.areas?.[0]?.decks ?? [])
const victoryLabel = computed(() =>
  game.victoryCondition
    ? t('game.piles.victoryCondition', {
        count: (game.victoryDisplay ?? []).filter((c) => cards.card(c.code)?.type === 'villain').length,
        condition: game.victoryCondition,
      })
    : t('game.piles.victoryDisplay'),
)
// the acceleration tokens adding threat to this area's main scheme: the first area's
const tokens = computed(() => area?.accelerationTokens ?? game.accelerationTokens ?? 0)
// and the acceleration icons on cards in play there
const icons = computed(() => area?.accelerationIcons ?? game.accelerationIcons ?? 0)
const deckLabel = (deck: DeckView) => {
  if (deck.kind === 'villain') return t('game.piles.villainDeck', { name: deck.name ?? '' })
  if (deck.kind === 'sideScheme') return t('game.piles.sideSchemeDeck')
  if (deck.kind === 'infinityStones') return t('game.piles.infinityStoneDeck')
  const key = `game.piles.named.${deck.name}`
  return te(key) ? t(key) : (deck.name ?? '')
}
const discardLabel = (deck: DeckView) => {
  if (deck.kind === 'villain') return t('game.piles.villainDiscard', { name: deck.name ?? '' })
  if (deck.kind === 'infinityStones') return t('game.piles.infinityStoneDiscard')
  return t('game.piles.sideSchemeDiscard')
}
// with villains' own decks the encounter deck is not used (The Wrecking Crew): shown only
// should a card end up in it
const encounterUnused = computed(
  () => decks.value.some((d) => d.kind === 'villain') && !game.encounterDeckSize && !(game.encounterDiscard ?? []).length,
)
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
    <div v-for="villain in villains" :key="villain.id" class="villain" :class="{ active: villains.length > 1 && villain.active }">
      <span v-if="villains.length > 1 && villain.active" class="active-flag">{{ t('game.activeVillain') }}</span>
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
      <!-- infinite hit points (Escape the Museum): a full bar, ∞ -->
      <Meter v-if="villain.infiniteHitPoints" :label="t('game.hp')" :value="1" :max="1" text="∞" tone="villain" />
      <Meter v-else :label="t('game.hp')" :value="Math.max(0, villain.hitPoints - villain.damage)" :max="villain.hitPoints" tone="villain" />
    </div>
    <!-- what is attached to the villain, as cards beside it -->
    <template v-for="villain in villains" :key="`${villain.id}-attached`">
      <BoardCard v-for="a in villain.attachments ?? []" :id="a.id" :key="a.id" class="villain-attached" :code="a.code" :name="a.name" :damage="a.damage ?? 0" :counters="a.counters || null" />
      <BoardCard v-for="u in villain.upgrades ?? []" :id="u.id" :key="u.id" class="villain-attached" :code="u.code" :name="u.name" :exhausted="u.exhausted" :counters="u.uses ?? null" />
    </template>

    <!-- environments in the middle, between the villain and the schemes -->
    <div v-if="environments.length" class="environments">
      <div v-for="environment in environments" :key="environment.id" class="scheme side">
        <BoardCard :id="environment.id" :code="environment.code" :name="environment.name" :attached="environment.attachments ?? []" :counters="environment.counters || null" :damage="environment.damage ?? 0" />
      </div>
    </div>

    <!-- the schemes, to the right beside the encounter deck -->
    <div class="schemes">
      <div v-for="scheme in mainSchemes" :key="scheme.id" class="scheme main">
        <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.threat" :flag="scheme.crisis ? t('game.crisis') : null" flag-icon="crisis" :counters="scheme.counters || null" :attached="[...(scheme.upgrades ?? []), ...(scheme.attachments ?? []), ...(scheme.attachedCards ?? [])]" :held="scheme.held || null" />
        <Meter :label="t('game.threat')" :value="scheme.threat" :max="target(scheme)" tone="threat" />
        <p v-if="escalation(scheme) !== null" class="escalation">
          {{ t('game.perRound', { count: escalation(scheme)! + tokens + icons }) }}
          <span v-if="icons" class="icons" :title="t('game.accelerationIcons', icons)">
            <GameIcon name="acceleration" :label="t('game.accelerationIcons', icons)" /><template v-if="icons > 1">×{{ icons }}</template>
          </span>
          <TokenStack
            v-if="tokens"
            kind="acceleration"
            :count="tokens"
            :title="t('game.acceleration', { count: tokens })"
          />
        </p>
      </div>

      <div v-if="sideSchemes.length" class="side-schemes">
        <div v-for="scheme in sideSchemes" :key="scheme.id" class="scheme side">
          <BoardCard :id="scheme.id" :code="scheme.code" :name="scheme.name" :threat="scheme.threat" :flag="scheme.crisis ? t('game.crisis') : null" flag-icon="crisis" :attached="[...(scheme.upgrades ?? []), ...(scheme.attachments ?? [])]" :held="scheme.held || null" />
        </div>
      </div>
    </div>

    <div class="encounter-piles">
      <template v-if="!encounterUnused">
        <Pile :label="t('game.piles.encounterDeck')" back="encounter" :count="game.encounterDeckSize ?? 0" pile="deck-encounter" />
        <Pile :label="t('game.piles.encounterDiscard')" :cards="game.encounterDiscard ?? []" pile="discard-encounter" />
      </template>
      <!-- the victory display: shared, out of play, the cards with victory X defeated -->
      <!-- a scenario won by villains in it (Loki) counts them against its victory condition -->
      <Pile v-if="game.victoryDisplay?.length || game.victoryCondition" :label="victoryLabel" :cards="game.victoryDisplay ?? []" pile="victory-display" />
      <!-- The Collection (Infiltrate the Museum): shared, out of play, its cards faceup -->
      <Pile v-if="game.collection?.length" :label="t('game.piles.collection')" :cards="game.collection" pile="collection" />
      <template v-for="deck in decks" :key="`${deck.kind}-${deck.name}`">
        <Pile :label="deckLabel(deck)" back="encounter" :count="deck.size" />
        <!-- a named deck's cards are discarded to the encounter discard pile -->
        <Pile v-if="deck.kind !== 'named'" :label="discardLabel(deck)" :cards="deck.discard" />
      </template>
    </div>
  </section>
</template>

<style scoped>
.villain-area {
  position: relative;
  display: flex;
  flex-wrap: wrap;
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
.villain { position: relative; display: flex; flex-direction: column; gap: 8px; flex: none; }
/* of several villains, the one that activates */
.villain.active .body :deep(.card) { outline: 2px solid #d6505a; outline-offset: 3px; }
.active-flag { position: absolute; z-index: 1; top: -8px; right: 0; padding: 1px 7px; border-radius: 999px; background: #b8323a; color: #fff; font-size: 0.58rem; font-weight: 800; letter-spacing: 0.1em; text-transform: uppercase; }
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
.escalation .icons { display: inline-flex; align-items: center; gap: 2px; font-weight: 700; color: var(--crimson-bright); }
.escalation { display: flex; align-items: center; gap: 6px; margin: 0; font-size: 0.7rem; color: var(--text-faint); --token-size: 20px; }
.side-schemes { display: flex; flex-wrap: wrap; gap: 10px; align-content: flex-start; flex: 0 1 auto; min-width: 0; }

.environments { display: flex; flex-wrap: wrap; gap: 10px; justify-content: center; margin-inline: auto; min-width: 0; }
.schemes { display: flex; gap: 18px; align-items: flex-start; margin-left: auto; min-width: 0; }
/* several decks wrap onto rows of their own, not over the schemes */
.encounter-piles { display: flex; flex-wrap: wrap; gap: 14px; flex: 0 1 auto; min-width: 0; padding-top: 4px; }
</style>
