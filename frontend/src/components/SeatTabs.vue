<script setup lang="ts">
// A tab a seat, for looking at another player's side of the table: each with its hero's
// portrait, hit points and hand, whose turn it is, and who is being asked.
import { LoaderCircle, Rows3 } from 'lucide-vue-next'
import { useCards } from '../cards'
import type { PlayerView } from '../server'

defineProps<{
  players: PlayerView[]
  viewing: string | null
  asked: string[]
  active: string | null
  /** your own seats at a game with friends */
  mine: string[]
  all: boolean
}>()
const emit = defineEmits<{ view: [playerId: string]; all: [] }>()
const cards = useCards()
// the hero face's art, whichever face shows
const portrait = (player: PlayerView) => {
  const code = player.form === 'hero' ? player.code : player.otherCode
  return `url('${cards.sources(code)[0]}')`
}
</script>

<template>
  <nav class="seat-tabs" role="tablist" :aria-label="$t('game.seats.label')">
    <button
      v-for="player in players"
      :key="player.id"
      type="button"
      role="tab"
      class="tab"
      :class="{ viewing: !all && player.id === viewing, active: player.id === active }"
      :aria-selected="!all && player.id === viewing"
      @click="emit('view', player.id)"
    >
      <span class="portrait square" :style="{ backgroundImage: portrait(player) }" />
      <span class="who">
        <span class="name">{{ player.name }}<span v-if="mine.includes(player.id)" class="you">{{ $t('game.lobby.you') }}</span></span>
        <span class="meta">
          <span class="hp">{{ Math.max(0, player.hitPoints - player.damage) }}/{{ player.hitPoints }}</span>
          · {{ $t('game.seats.hand', { count: player.handCount }) }}
        </span>
      </span>
      <span v-if="player.id === active" class="turn" :title="$t('game.seats.turn')">{{ $t('game.seats.turn') }}</span>
      <LoaderCircle v-if="asked.includes(player.id)" class="asked" :size="16" :aria-label="$t('game.seats.asked')" />
    </button>
    <button type="button" class="tab all" :class="{ viewing: all }" :title="$t('game.seats.all')" @click="emit('all')">
      <Rows3 :size="16" aria-hidden="true" />
      {{ $t('game.seats.all') }}
    </button>
  </nav>
</template>

<style scoped>
.seat-tabs { display: flex; gap: 6px; overflow-x: auto; flex: none; padding-bottom: 2px; }
.tab {
  position: relative;
  display: flex;
  align-items: center;
  gap: 9px;
  padding: 5px 12px 5px 5px;
  border: 1px solid var(--line);
  border-radius: 10px;
  background: var(--ink-2);
  color: var(--text-muted);
  font: inherit;
  text-align: left;
  white-space: nowrap;
  cursor: pointer;
  transition: border-color 0.15s, background 0.15s;
}
.tab:hover { background: var(--ink-3); color: var(--text); }
.tab.viewing { border-color: var(--steel); background: linear-gradient(rgb(91 141 184 / 0.16), rgb(91 141 184 / 0.06)), var(--ink-2); color: var(--text); }
.portrait { width: 34px; height: 34px; border-radius: 8px; }
.who { display: flex; flex-direction: column; line-height: 1.2; }
.name { font-weight: 700; font-size: 0.85rem; color: var(--text); }
.you { margin-left: 6px; font-size: 0.6rem; font-weight: 800; letter-spacing: 0.1em; text-transform: uppercase; color: var(--steel-bright); }
.meta { font-size: 0.7rem; color: var(--text-faint); }
.hp { color: #9fd8ae; font-weight: 600; }
.turn { padding: 1px 6px; border-radius: 999px; background: var(--steel-deep); color: var(--steel-bright); font-size: 0.6rem; font-weight: 800; letter-spacing: 0.08em; text-transform: uppercase; }
.asked { color: var(--interactive); animation: spin 2.5s linear infinite; }
.tab.all { padding: 5px 12px; font-size: 0.75rem; font-weight: 600; }
@keyframes spin { to { rotate: 360deg; } }
</style>
