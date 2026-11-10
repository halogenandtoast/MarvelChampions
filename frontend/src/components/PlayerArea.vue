<script setup lang="ts">
// One player's side of the table: the identity on whichever face shows, the minions
// engaged with them, their allies, supports and upgrades, and their deck and discard.
import { computed, useTemplateRef } from 'vue'
import { useFitToWidth } from '../composables/useFitToWidth'
import { useI18n } from 'vue-i18n'
import type { EnvironmentView, MinionView, PlayerView, SupportView, UpgradeView } from '../server'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import Pile from './Pile.vue'
import { useCards } from '../cards'
import { useChoices } from '../composables/useChoices'

const area = useTemplateRef<HTMLElement>('area')
const tableau = useTemplateRef<HTMLElement>('tableau')
const { scale, width: tableauWidth } = useFitToWidth(area, tableau)

const { player, minions = [], environments = [], active = false, compact = false, riverEnemyIds = [] } = defineProps<{
  riverEnemyIds?: string[]
  player: PlayerView
  minions?: MinionView[]
  /** the environments in their play area (Ebony Maw's Spells) */
  environments?: EnvironmentView[]
  /** whose turn it is */
  active?: boolean
  /** one of several shown at once: smaller */
  compact?: boolean
}>()
const { t } = useI18n()
const { assignedDamage } = useChoices()

const openAllySlots = computed(() => Math.max(0, (player.allyLimit ?? 0) - (player.allies?.length ?? 0)))
const hero = computed(() => player.form === 'hero')
const cards = useCards()
const unfolded = computed(() => player.faces.length === 3 && player.code === player.faces[2])
const horizontal = computed(() => unfolded.value && cards.landscape(player.code))
const stats = computed(() =>
  (
    [
      ['thw', player.thwart],
      ['atk', player.attack],
      ['def', player.defense],
      ['rec', player.recover],
    ] as const
  ).filter(([, v]) => v !== null && v !== undefined),
)
type TableCard = SupportView & Partial<Pick<UpgradeView, 'resourceSlots' | 'occupiedSlots' | 'heldCardsFacedown' | 'attachedTo'>>
// SP//dr's other physical identity card remains an interactive support/upgrade.
const separatedIdentity = computed(() => player.faces.includes('31001a') && player.faces.includes('31002a'))
const identityCompanion = computed<TableCard | undefined>(() => separatedIdentity.value
  ? [...(player.supports ?? []), ...(player.upgrades ?? [])].find(card => card.code === '31001b' || card.code === '31002b')
  : undefined)
const supports = computed<TableCard[]>((previous) => {
  const rank = new Map(previous?.map((card, i) => [card.id, i]))
  return [...(player.supports ?? []), ...(player.upgrades ?? []), ...(player.attachments ?? []), ...(player.obligations ?? [])]
    .filter(card => card.id !== identityCompanion.value?.id)
    .sort((a, b) => (rank.get(a.id) ?? Infinity) - (rank.get(b.id) ?? Infinity))
})
// cards attached to a support facedown, shown as such
const facedown = (card: { id: string; facedownCards?: number; heldCardsFacedown?: number }) =>
  Array.from({ length: card.facedownCards ?? card.heldCardsFacedown ?? 0 }, (_, n) => ({ id: `${card.id}-facedown-${n}`, code: '', name: t('game.facedownCard') }))
const encounterHolders = computed(() => [...(player.supports ?? []), ...(player.obligations ?? [])].filter(c => c.tuckedCards?.length))
const holders = computed(() => [...(player.upgrades ?? []), ...(player.supports ?? [])].filter((u) => u.heldCards?.length))
const detachedMinions = computed(() => minions.filter((m) => !m.attachedToMinion))
const minionAttached = (m: MinionView) => [...minions.filter((a) => a.attachedToMinion === m.id), ...(m.attachments ?? []), ...(m.upgrades ?? []), ...(m.heldCards ?? []), ...facedown(m)]
</script>

<template>
  <section ref="area" class="player-area" :class="{ active, compact, hero, unfolded, horizontal, 'alter-ego': !hero, 'separated-identity': !!identityCompanion }">
    <div ref="tableau" class="player-tableau" :style="{ zoom: scale, width: tableauWidth === undefined ? undefined : `${tableauWidth}px` }">
    <div class="identity-column">
      <div class="identity">
        <div class="body">
          <dl class="stats">
            <div v-for="[stat, value] in stats" :key="stat" :class="`stat-${stat}`"><dt>{{ t(`game.stats.${stat}`) }}</dt><dd>{{ value }}</dd></div>
            <div><dt>{{ t('game.stats.hand') }}</dt><dd>{{ player.handSize }}</dd></div>
          </dl>
          <BoardCard
            :id="player.id"
            :code="player.code"
            :faces="separatedIdentity ? undefined : player.faces"
            :name="player.name"
            :exhausted="player.exhausted"
            :damage="player.damage"
            :threat="player.threat || null"
            :stun-cards="player.stunCards"
            :confuse-cards="player.confuseCards"
            :stunned="player.stunned"
            :confused="player.confused"
            :tough="player.tough"
            :tough-cards="player.toughCards"
            :counters="player.counters || null"
          />
          <BoardCard
            v-if="identityCompanion"
            :id="identityCompanion.id"
            :code="identityCompanion.code"
            :name="identityCompanion.name"
            :exhausted="identityCompanion.exhausted"
            :counters="identityCompanion.uses ?? identityCompanion.counters ?? null"
            :resource-slots="identityCompanion.resourceSlots"
            :occupied-slots="identityCompanion.occupiedSlots"
            :damage="identityCompanion.damage || 0"
            :threat="identityCompanion.threat || null"
            :attached="facedown(identityCompanion)"
          />
        </div>
        <Meter :label="t('game.hp')" :value="Math.max(0, player.hitPoints - player.damage)" :subtracting="assignedDamage[player.id] ?? 0" :max="player.hitPoints" />
      </div>
      <!-- what this player is asked sits beneath them, at the foot of the area -->
      <div v-if="$slots.default" class="asked"><slot /></div>
    </div>

    <div v-if="detachedMinions.length" class="zone engaged">
      <span class="zone-label">{{ t('game.zones.engaged') }}</span>
      <div class="cards">
        <div v-for="minion in detachedMinions" :key="minion.id" class="minion">
          <div class="body">
            <dl class="stats">
              <div class="stat-sch"><dt>{{ t('game.stats.sch') }}</dt><dd>{{ minion.scheme ?? '–' }}</dd></div>
              <div class="stat-atk"><dt>{{ t('game.stats.atk') }}</dt><dd>{{ minion.attack ?? '–' }}</dd></div>
            </dl>
            <BoardCard
              :id="minion.id"
              :code="minion.code"
              :name="minion.name"
              :exhausted="minion.exhausted"
              :damage="minion.damage" :counters="minion.counters || null"
            :threat="minion.threat || null"
              :stun-cards="minion.stunCards"
              :confuse-cards="minion.confuseCards"
              :stunned="minion.stunned"
              :confused="minion.confused"
              :tough="minion.tough"
              :tough-cards="minion.toughCards"
              :bomb-counters="minion.bombCounters"
              :attached="minionAttached(minion)"
            />
            <Pile v-if="minion.boostCards && !riverEnemyIds.includes(minion.id)" class="boost-pile" :label="t('game.boost')" back="encounter" :count="minion.boostCards" />
          </div>
          <Meter :label="t('game.hp')" :value="Math.max(0, minion.hitPoints - minion.damage)" :subtracting="assignedDamage[minion.id] ?? 0" :max="minion.hitPoints" tone="villain" />
        </div>
      </div>
    </div>

    <div v-if="environments.length" class="zone environments">
      <span class="zone-label">{{ t('game.zones.environments') }}</span>
      <div class="cards">
        <BoardCard
          v-for="environment in environments"
          :id="environment.id"
          :key="environment.id"
          :code="environment.code"
          :name="environment.name"
          :attached="environment.attachments ?? []"
          :counters="environment.counters || null"
        />
      </div>
    </div>

    <div class="zone allies">
      <span class="zone-label">{{ t('game.zones.allies') }}</span>
      <div class="cards">
        <div v-for="ally in player.allies ?? []" :key="ally.id" class="ally">
          <BoardCard
            :id="ally.id"
            :code="ally.code"
            :name="ally.name"
            :tucked-boost-icons="ally.tuckedBoostIcons"
            :tucked-boost-star="ally.tuckedBoostStar"
            :exhausted="ally.exhausted"
            :damage="ally.damage"
            :threat="ally.threat || null"
            :counters="ally.counters ?? null"
            :stun-cards="ally.stunCards"
            :confuse-cards="ally.confuseCards"
            :tough-cards="ally.toughCards"
            :stunned="ally.stunned"
            :confused="ally.confused"
            :tough="ally.tough"
            :attached="[...(ally.upgrades ?? []), ...(ally.attachments ?? []), ...facedown(ally)]"
          />
          <Meter :label="t('game.hp')" :value="Math.max(0, ally.hitPoints - ally.damage)" :subtracting="assignedDamage[ally.id] ?? 0" :max="ally.hitPoints" />
        </div>
        <!-- a free slot for each ally still allowed -->
        <span v-for="n in openAllySlots" :key="`slot-${n}`" class="ally-slot" />
      </div>
    </div>

    <div class="zone supports" :class="{ empty: !supports.length }">
      <span class="zone-label">{{ t('game.zones.supports') }}</span>
      <div class="cards">
        <BoardCard
          v-for="card in supports"
          :id="card.id"
          :key="card.id"
          :code="card.code"
          :name="card.name"
          :exhausted="card.exhausted"
          :counters="card.uses ?? card.counters ?? null"
          :resource-slots="card.resourceSlots"
          :occupied-slots="card.occupiedSlots"
          :damage="card.damage || 0"
          :threat="card.threat || null"
          :attached="facedown(card)"
          :flag="card.facedown ? t('game.facedownCard') : card.attachedTo ? t('game.attachedTo', { name: card.attachedTo.name }) : null"
          :class="{ 'facedown-upgrade': card.facedown }"
        />
      </div>
    </div>

    <div class="side">
      <div class="piles">
        <Pile :label="t('game.piles.deck')" :back="player.deckTop ? null : 'player'" :cards="player.deckTop ? [player.deckTop] : null" :count="player.deckSize" :pile="`deck-${player.id}`" />
        <Pile :label="t('game.piles.discard')" :cards="player.discard ?? []" :pile="`discard-${player.id}`" />
        <Pile v-if="player.weatherDeckSize != null" :label="t('game.piles.weather')" back="player" :count="player.weatherDeckSize" />
        <Pile v-if="player.senseDeck?.length" :label="t('game.piles.sense')" :cards="player.senseDeck" />
        <Pile v-if="player.tuckedPlayerCards?.length" :label="t('game.piles.tuckedPlayerCards')" :cards="player.tuckedPlayerCards" />
        <Pile v-if="player.laborDeckSize != null" :label="t('game.piles.labor')" back="encounter" :count="player.laborDeckSize" />
        <Pile v-if="player.giftDeckSize != null" :label="t('game.piles.gift')" back="player" :count="player.giftDeckSize" />
        <template v-if="player.invocation">
          <Pile :label="t('game.piles.invocation')" :count="player.invocation.deckSize" :cards="player.invocation.top ? [player.invocation.top] : []" />
          <Pile :label="t('game.piles.invocationDiscard')" :cards="player.invocation.discard" />
        </template>
        <Pile v-for="u in holders" :key="u.id" :label="u.name" :cards="u.heldCards ?? []" />
        <Pile v-if="player.tuckedEncounterCards?.length" :label="t('game.piles.tuckedEncounterCards')" :cards="player.tuckedEncounterCards" />
        <Pile v-for="holder in encounterHolders" :key="`tucked-${holder.id}`" :label="holder.name" :cards="holder.tuckedCards ?? []" />
        <Pile v-if="player.identityBoostCards" :label="t('game.piles.identityBoostCards')" back="encounter" :count="player.identityBoostCards" />
      </div>
      <!-- encounter cards dealt face down, in the corner, above the hand -->
      <Pile v-if="player.dealtEncounterCards" class="dealt" :label="t('game.piles.dealt')" back="encounter" :count="player.dealtEncounterCards" :pile="`dealt-${player.id}`" />
    </div>
    </div>
  </section>
</template>

<style scoped>
.player-area {
  --tone: var(--steel);
  width: 100%;
  min-width: 0;
  max-width: 100%;
  padding: 14px 16px;
  border-radius: 14px;
  background:
    linear-gradient(100deg, rgb(91 141 184 / 0.12), transparent 50%),
    linear-gradient(var(--ink-2), var(--ink-2));
  border: 1px solid rgb(91 141 184 / 0.2);
  overflow-x: auto;
}
.player-tableau { display: flex; align-items: flex-start; gap: 14px; width: max-content; }
.player-tableau > .zone, .player-tableau > .zone.allies { flex: none; }
.player-tableau > .zone.supports { margin-left: auto; }
.player-tableau .cards { flex-wrap: nowrap; }
.player-area.active { border-color: rgb(91 141 184 / 0.5); box-shadow: 0 0 0 1px rgb(91 141 184 / 0.25), 0 8px 30px rgb(91 141 184 / 0.08); }
.identity-column { display: flex; flex-direction: column; gap: 12px; flex: none; align-self: stretch; width: calc(var(--card-width) + 164px); transition: width 0.4s ease; }
.player-area.unfolded .identity-column { width: calc(var(--card-width) / var(--card-aspect) + 164px); }
.player-area.unfolded .identity > :deep(.meter) { width: calc(var(--card-width) / var(--card-aspect)); }
.player-area.horizontal .identity-column { width: calc(2 * var(--card-width) + 164px); }
.player-area.horizontal .identity > :deep(.meter) { width: calc(2 * var(--card-width)); }
.player-area.separated-identity .identity-column { width: calc(2 * var(--card-width) + 170px); }
.player-area.separated-identity .identity > :deep(.meter) { width: calc(2 * var(--card-width) + 6px); }
/* above the hand that floats over the foot of the board */
.asked { position: relative; z-index: 1; display: flex; flex-direction: column; gap: 10px; margin-top: auto; }
.identity { display: flex; flex-direction: column; gap: 8px; align-self: flex-start; }
/* a card's stats pinned to its left */
.body { display: flex; gap: 6px; align-items: flex-start; }
/* health under the card alone, not its stats */
.identity > :deep(.meter), .minion > :deep(.meter) { width: var(--card-width); align-self: flex-end; transition: width 0.4s ease; }
.stats { display: flex; flex-direction: column; gap: 4px; margin: 0; width: 44px; flex: none; }
.stats div { padding: 5px 0 4px; border-radius: 7px; background: var(--stat-bg, var(--ink-0)); border: 1px solid var(--stat-bg, var(--line)); text-align: center; }
.stats dt { font-size: 0.5rem; font-weight: 700; letter-spacing: 0.12em; color: var(--stat-label, var(--text-faint)); }
.stats dd { margin: 0; font-family: var(--font-display); font-size: 1.2rem; font-weight: 800; line-height: 1.05; color: var(--stat-fg, inherit); }

.zone {
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 6px;
  min-width: 0;
  flex: 0 1 auto;
}
/* an energy form she is not in: turned down, still hers to read */
.facedown-upgrade { opacity: 0.55; filter: grayscale(0.6); }
.zone-label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--text-faint); }
.cards { display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-start; }
.zone.empty .cards {
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  border: 1px dashed var(--line);
  border-radius: 8px;
}
.zone.engaged {
  padding: 6px 8px 8px;
  border-radius: 10px;
  background: rgb(184 50 58 / 0.08);
  border: 1px solid rgb(184 50 58 / 0.25);
}
.minion { display: flex; flex-direction: column; gap: 6px; }
.minion .boost-pile { --card-width: calc(var(--attached-width, 100px)); }
.minion:has(.boost-pile) > :deep(.meter) { align-self: flex-start; margin-left: 50px; }
.zone.engaged .zone-label { color: #e48b91; }
.zone.allies { flex: 1 1 auto; }
.ally { display: flex; flex-direction: column; gap: 6px; width: var(--card-width); }
.ally-slot { width: var(--card-width); aspect-ratio: var(--card-aspect); border: 1px dashed var(--line); border-radius: 8px; }
.side { display: flex; flex-direction: column; align-items: flex-end; gap: 14px; flex: none; align-self: stretch; }
.piles { display: flex; gap: 10px; padding-top: 18px; }
.dealt { position: relative; z-index: 1; margin-top: auto; }
.compact { padding: 10px 12px; }
@media (max-width: 760px) {
  .player-area { padding: 12px; overflow: visible; }
  /* Reflow groups at their readable size; desktop fitting must not shrink prompts. */
  .player-tableau { zoom: 1 !important; width: 100% !important; flex-wrap: wrap; gap: 16px 12px; }
  .identity-column, .player-area.unfolded .identity-column, .player-area.horizontal .identity-column, .player-area.separated-identity .identity-column { width: 100%; }
  .player-tableau > .zone, .player-tableau > .zone.allies { flex: 1 1 100%; max-width: 100%; }
  .player-tableau .cards { flex-wrap: wrap; gap: 14px; }
  .player-tableau > .zone.supports { margin-left: 0; }
  .zone-label { font-size: 0.75rem; color: var(--text-muted); }
  .zone.empty, .ally-slot { display: none; }
  .side { width: 100%; align-items: flex-start; }
  .piles { flex-wrap: wrap; padding-top: 0; }
  .stats dt { font-size: 0.65rem; }
  .stats dd { font-size: 1.4rem; }
  .asked:empty { display: none; }
}
</style>
