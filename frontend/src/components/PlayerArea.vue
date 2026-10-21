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
    <div class="identity">
      <BoardCard
        :id="player.id"
        :code="player.code"
        :name="player.name"
        :size="compact ? 'md' : 'lg'"
        :exhausted="player.exhausted"
        :damage="player.damage"
        :stunned="player.stunned"
        :confused="player.confused"
        :tough="player.tough"
      />
      <div class="identity-info">
        <p class="who">
          <span class="form">{{ t(hero ? 'game.form.hero' : 'game.form.alterEgo') }}</span>
          <span class="player-name">{{ player.name }}</span>
        </p>
        <Meter :label="t('game.hp')" :value="Math.max(0, player.hitPoints - player.damage)" :max="player.hitPoints" />
        <dl class="stats">
          <div v-for="[stat, value] in stats" :key="stat"><dt>{{ t(`game.stats.${stat}`) }}</dt><dd>{{ value }}</dd></div>
          <div><dt>{{ t('game.stats.hand') }}</dt><dd>{{ player.handSize }}</dd></div>
        </dl>
      </div>
    </div>

    <div v-if="minions.length" class="zone engaged">
      <span class="zone-label">{{ t('game.zones.engaged') }}</span>
      <div class="cards">
        <BoardCard
          v-for="minion in minions"
          :id="minion.id"
          :key="minion.id"
          :code="minion.code"
          :name="minion.name"
          :size="compact ? 'sm' : 'md'"
          :damage="minion.damage"
          :hit-points="minion.hitPoints"
          :stunned="minion.stunned"
          :confused="minion.confused"
          :tough="minion.tough"
          :attached="minionAttached(minion)"
        />
      </div>
    </div>

    <div class="zone allies" :class="{ empty: !player.allies?.length }">
      <span class="zone-label">{{ t('game.zones.allies') }}</span>
      <div class="cards">
        <BoardCard
          v-for="ally in player.allies ?? []"
          :id="ally.id"
          :key="ally.id"
          :code="ally.code"
          :name="ally.name"
          :size="compact ? 'sm' : 'md'"
          :exhausted="ally.exhausted"
          :damage="ally.damage"
          :hit-points="ally.hitPoints"
          :counters="ally.counters ?? null"
          :stunned="ally.stunned"
          :confused="ally.confused"
          :tough="ally.tough"
          :attached="ally.upgrades ?? []"
        />
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
          :size="compact ? 'xs' : 'sm'"
          :exhausted="card.exhausted"
          :counters="card.uses ?? null"
        />
      </div>
    </div>

    <div class="piles">
      <Pile :label="t('game.piles.deck')" back="player" :count="player.deckSize" size="xs" />
      <Pile :label="t('game.piles.discard')" :cards="player.discard ?? []" size="xs" />
      <Pile v-if="player.dealtEncounterCards" :label="t('game.piles.dealt')" back="encounter" :count="player.dealtEncounterCards" size="xs" />
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
.identity { display: flex; gap: 12px; align-items: flex-start; flex: none; }
.identity-info { display: flex; flex-direction: column; gap: 10px; width: 152px; padding-top: 2px; }
.who { display: flex; flex-direction: column; gap: 2px; margin: 0; }
.form { font-size: 0.6rem; font-weight: 800; letter-spacing: 0.14em; text-transform: uppercase; color: var(--steel-bright); }
.alter-ego .form { color: var(--text-faint); }
.player-name { font-family: var(--font-display); font-size: 1.15rem; font-weight: 700; line-height: 1.05; color: var(--title); overflow: hidden; text-overflow: ellipsis; }
.stats { display: grid; grid-template-columns: repeat(auto-fill, minmax(34px, 1fr)); gap: 5px; margin: 0; }
.stats div { padding: 5px 0 4px; border-radius: 7px; background: var(--ink-0); border: 1px solid var(--line); text-align: center; }
.stats dt { font-size: 0.55rem; font-weight: 700; letter-spacing: 0.12em; color: var(--text-faint); }
.stats dd { margin: 0; font-family: var(--font-display); font-size: 1.2rem; font-weight: 800; line-height: 1.05; }

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
  width: calc(var(--card-width) * 0.82);
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
.zone.engaged .zone-label { color: #e48b91; }
.zone.allies { flex: 1 1 auto; }
.piles { display: flex; gap: 10px; margin-left: auto; flex: none; padding-top: 18px; }
.compact { padding: 10px 12px; }
</style>
