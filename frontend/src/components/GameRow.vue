<script lang="ts" setup>
import { computed, useTemplateRef } from 'vue'
import { Trash2 } from 'lucide-vue-next'
import ConfirmDelete from './ConfirmDelete.vue'
import HeroPortrait from './HeroPortrait.vue'
import { useCards } from '../cards'
import type { GameSummary } from '../server'

const props = defineProps<{ game: GameSummary; deleteGame?: () => void; spectate?: boolean }>()

const confirm = useTemplateRef<InstanceType<typeof ConfirmDelete>>('confirm')
const cards = useCards()
const villainArt = computed(() => cards.sources(props.game.villain).map((s) => `url('${s}')`).join(', '))
const link = computed(() => (props.spectate ? `/games/${props.game.id}/spectate` : `/games/${props.game.id}`))
</script>

<template>
  <article class="game-row" :class="{ won: game.outcome === 'Won', lost: game.outcome === 'Lost' }">
    <RouterLink class="villain portrait" :to="link" :style="{ backgroundImage: villainArt }" :aria-label="game.scenario" />
    <div class="body">
      <div class="title-line">
        <RouterLink class="title" :to="link">{{ game.name }}</RouterLink>
        <span v-if="game.outcome" class="badge" :class="game.outcome.toLowerCase()">{{ $t(game.outcome === 'Won' ? 'home.won' : game.outcome === 'Lost' ? 'home.lost' : `game.outcome.${game.outcome}`) }}</span>
        <span v-if="game.difficulty === 'Expert'" class="badge expert">{{ $t('difficulty.Expert') }}</span>
      </div>
      <p class="meta">
        <span class="vs">{{ $t('home.versus', { villain: game.scenario }) }}</span>
        <template v-if="game.waiting"> · {{ $t('home.waitingForPlayers') }}</template>
        <template v-else> · {{ $t('game.round', { round: game.round }) }} · {{ $t(`game.phases.${game.phase}`) }}</template>
      </p>
      <ul v-if="game.seats?.length" class="seats">
        <li v-for="(seat, n) in game.seats" :key="n" class="seat" :class="{ eliminated: seat.eliminated }">
          <HeroPortrait class="face square" :code="seat.identity" />
          <span class="seat-name">{{ seat.name }}</span>
          <span v-if="seat.damage" class="seat-damage">−{{ seat.damage }}</span>
        </li>
      </ul>
      <p v-else class="meta">{{ game.playerName }}</p>
    </div>
    <button v-if="deleteGame" type="button" class="delete" :aria-label="$t('home.deleteGame')" :title="$t('home.deleteGame')" @click="confirm?.open()">
      <Trash2 aria-hidden="true" />
    </button>
    <ConfirmDelete v-if="deleteGame" ref="confirm" :title="$t('home.confirmDelete')" @confirm="deleteGame()">
      <span class="confirm-art portrait square" :style="{ backgroundImage: villainArt }" />
      <span class="confirm-details">
        <strong>{{ game.name }}</strong>
        <span class="confirm-meta">{{ $t('home.versus', { villain: game.scenario }) }}</span>
        <span v-if="game.seats?.length" class="confirm-meta">{{ game.seats.map((seat) => seat.name).join(', ') }}</span>
      </span>
    </ConfirmDelete>
  </article>
</template>

<style scoped>
.game-row {
  --edge: var(--crimson);
  position: relative;
  display: flex;
  align-items: stretch;
  min-height: 104px;
  border: 1px solid var(--line);
  border-radius: 14px;
  background: var(--ink-2);
  overflow: hidden;
  transition: border-color 0.15s, transform 0.15s;
}
.game-row:hover { border-color: var(--line-strong); }
.game-row.won { --edge: var(--health); }
.game-row.lost { --edge: var(--ink-5); }
.villain {
  position: relative;
  flex: 0 0 150px;
}
/* the villain fades into the row */
.villain::after { content: ''; position: absolute; inset: 0; background: linear-gradient(90deg, transparent 40%, var(--ink-2)); }
.villain::before { content: ''; position: absolute; inset: 0 auto 0 0; width: 4px; z-index: 1; background: var(--edge); }
.lost .villain { filter: grayscale(0.7) brightness(0.8); }
.body { flex: 1; min-width: 0; display: flex; flex-direction: column; justify-content: center; gap: 4px; padding: 12px 16px 12px 4px; }
.title-line { display: flex; align-items: center; gap: 10px; min-width: 0; }
.title { font-family: var(--font-display); font-size: 1.5rem; font-weight: 700; line-height: 1.1; color: var(--title); text-decoration: none; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.title:hover { color: #fff; text-decoration: underline; text-decoration-color: var(--crimson); text-underline-offset: 4px; }
.badge { flex: none; padding: 2px 8px; border-radius: 999px; font-size: 0.65rem; font-weight: 800; letter-spacing: 0.1em; text-transform: uppercase; }
.badge.won { background: var(--health-deep); color: #9fd8ae; }
.badge.lost { background: var(--ink-4); color: var(--text-muted); }
.badge.expert { background: var(--crimson-deep); color: #ffb3b7; }
.meta { margin: 0; font-size: 0.8rem; color: var(--text-muted); }
.vs { color: var(--text); font-weight: 600; }
.seats { display: flex; flex-wrap: wrap; gap: 8px 14px; margin: 6px 0 0; padding: 0; list-style: none; }
.seat { display: flex; align-items: center; gap: 7px; }
.seat.eliminated { opacity: 0.4; }
.face { width: 30px; height: 30px; border-radius: 8px; overflow: hidden; }
.face::after { height: 0; }
.seat-name { font-size: 0.82rem; font-weight: 600; }
.seat-damage { font-size: 0.75rem; font-weight: 700; color: #f0a4a9; }
.delete { align-self: center; margin-right: 14px; padding: 8px; border: 0; border-radius: 8px; background: none; color: var(--text-faint); cursor: pointer; }
.delete:hover { color: var(--danger); background: rgb(214 80 88 / 0.1); }
.delete svg { width: 18px; height: 18px; }
.confirm-art { flex: 0 0 64px; height: 64px; border-radius: 10px; }
.confirm-details { display: flex; flex-direction: column; gap: 2px; min-width: 0; }
.confirm-meta { color: var(--text-muted); font-size: 0.85em; }
@media (max-width: 600px) {
  .villain { flex-basis: 84px; }
}
</style>
