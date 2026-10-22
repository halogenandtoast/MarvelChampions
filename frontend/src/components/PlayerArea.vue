<script setup lang="ts">
// One player's side of the table: the identity on whichever face shows, the minions
// engaged with them, their allies, supports and upgrades, and their deck and discard.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { MinionView, PlayerView } from '../server'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'
import Pile from './Pile.vue'

const { player, minions = [], active = false, compact = false } = defineProps<{
  player: PlayerView
  minions?: MinionView[]
  /** whose turn it is */
  active?: boolean
  /** one of several shown at once: smaller */
  compact?: boolean
}>()
const { t } = useI18n()

const openAllySlots = computed(() => Math.max(0, (player.allyLimit ?? 0) - (player.allies?.length ?? 0)))
const hero = computed(() => player.form === 'hero')
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
const supports = computed(() => [...(player.supports ?? []), ...(player.upgrades ?? [])])
const minionAttached = (m: MinionView) => [...(m.attachments ?? []), ...(m.upgrades ?? [])]
</script>

<template>
  <section class="player-area" :class="{ active, compact, hero, 'alter-ego': !hero }">
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
            :name="player.name"
            :exhausted="player.exhausted"
            :damage="player.damage"
            :stunned="player.stunned"
            :confused="player.confused"
            :tough="player.tough"
          />
        </div>
        <Meter :label="t('game.hp')" :value="Math.max(0, player.hitPoints - player.damage)" :max="player.hitPoints" />
      </div>
      <!-- what this player is asked sits beneath them, at the foot of the area -->
      <div v-if="$slots.default" class="asked"><slot /></div>
    </div>

    <div v-if="minions.length" class="zone engaged">
      <span class="zone-label">{{ t('game.zones.engaged') }}</span>
      <div class="cards">
        <div v-for="minion in minions" :key="minion.id" class="minion">
          <div class="body">
            <dl class="stats">
              <div class="stat-sch"><dt>{{ t('game.stats.sch') }}</dt><dd>{{ minion.scheme }}</dd></div>
              <div class="stat-atk"><dt>{{ t('game.stats.atk') }}</dt><dd>{{ minion.attack }}</dd></div>
            </dl>
            <BoardCard
              :id="minion.id"
              :code="minion.code"
              :name="minion.name"
              :damage="minion.damage"
              :stunned="minion.stunned"
              :confused="minion.confused"
              :tough="minion.tough"
              :attached="minionAttached(minion)"
            />
          </div>
          <Meter :label="t('game.hp')" :value="Math.max(0, minion.hitPoints - minion.damage)" :max="minion.hitPoints" tone="villain" />
        </div>
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
            :exhausted="ally.exhausted"
            :damage="ally.damage"
            :counters="ally.counters ?? null"
            :stunned="ally.stunned"
            :confused="ally.confused"
            :tough="ally.tough"
            :attached="ally.upgrades ?? []"
          />
          <Meter :label="t('game.hp')" :value="Math.max(0, ally.hitPoints - ally.damage)" :max="ally.hitPoints" />
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
          :counters="card.uses ?? null"
          :threat="card.threat || null"
        />
      </div>
    </div>

    <div class="piles">
      <Pile :label="t('game.piles.deck')" back="player" :count="player.deckSize" :pile="`deck-${player.id}`" />
      <Pile :label="t('game.piles.discard')" :cards="player.discard ?? []" :pile="`discard-${player.id}`" />
      <Pile v-if="player.dealtEncounterCards" :label="t('game.piles.dealt')" back="encounter" :count="player.dealtEncounterCards" />
    </div>
  </section>
</template>

<style scoped>
.player-area {
  --tone: var(--steel);
  display: flex;
  align-items: flex-start;
  gap: 14px;
  padding: 14px 16px;
  border-radius: 14px;
  background:
    linear-gradient(100deg, rgb(91 141 184 / 0.12), transparent 50%),
    linear-gradient(var(--ink-2), var(--ink-2));
  border: 1px solid rgb(91 141 184 / 0.2);
  overflow-x: auto;
}
.player-area.active { border-color: rgb(91 141 184 / 0.5); box-shadow: 0 0 0 1px rgb(91 141 184 / 0.25), 0 8px 30px rgb(91 141 184 / 0.08); }
.identity-column { display: flex; flex-direction: column; gap: 12px; flex: none; align-self: stretch; width: calc(var(--card-width) + 164px); }
/* above the hand that floats over the foot of the board */
.asked { position: relative; z-index: 1; display: flex; flex-direction: column; gap: 10px; margin-top: auto; }
.identity { display: flex; flex-direction: column; gap: 8px; align-self: flex-start; }
/* a card's stats pinned to its left */
.body { display: flex; gap: 6px; align-items: flex-start; }
/* health under the card alone, not its stats */
.identity > :deep(.meter), .minion > :deep(.meter) { width: var(--card-width); align-self: flex-end; }
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
.zone.engaged .zone-label { color: #e48b91; }
.zone.allies { flex: 1 1 auto; }
.ally { display: flex; flex-direction: column; gap: 6px; width: var(--card-width); }
.ally-slot { width: var(--card-width); aspect-ratio: var(--card-aspect); border: 1px dashed var(--line); border-radius: 8px; }
.piles { display: flex; gap: 10px; margin-left: auto; flex: none; padding-top: 18px; }
.compact { padding: 10px 12px; }
</style>
