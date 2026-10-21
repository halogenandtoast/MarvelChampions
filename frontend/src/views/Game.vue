<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { useServerText } from '../i18n'
import { currentUser } from '../auth'
import { exportGame } from '../api'
import { useGame } from '../composables/useGame'
import { provideChoices } from '../composables/useChoices'
import type { CardView, ChoiceView, PlayerView } from '../server'
import BoardCard from '../components/BoardCard.vue'
import CardOverlay from '../components/CardOverlay.vue'
import DebugDialog from '../components/DebugDialog.vue'
import FileBug from '../components/FileBug.vue'
import GameBar from '../components/GameBar.vue'
import GameLog from '../components/GameLog.vue'
import HandFan from '../components/HandFan.vue'
import Lobby from '../components/Lobby.vue'
import PlayerArea from '../components/PlayerArea.vue'
import PayingTray from '../components/PayingTray.vue'
import QuestionZone from '../components/QuestionZone.vue'
import SeatTabs from '../components/SeatTabs.vue'
import VillainArea from '../components/VillainArea.vue'

const route = useRoute()
const { t } = useI18n()
const tr = useServerText()
const spectate = route.meta.spectate === true
const gameId = route.params.id as string
const { error, game, data, log, hasOlder, loadOlder, say, undoTo, started, table, answer, reset, undo, canUndo, debug, join, leave, start } =
  useGame(gameId, spectate)

const players = computed<PlayerView[]>(() => game.value?.players ?? [])
const playerName = computed(() => Object.fromEntries(players.value.map((p) => [p.id, p.name])))

// With friends you play your own seats; otherwise the owner plays every seat.
const withFriends = computed(() => table.value?.withFriends ?? false)
const owner = computed(() => table.value?.owner === currentUser.value?.id)
const guest = computed(() => withFriends.value && !owner.value)
const mySeats = computed<string[]>(() => {
  if (spectate || !game.value) return []
  const seats = game.value.seats ?? players.value.map((p) => p.id)
  if (!withFriends.value) return owner.value ? seats : []
  return (table.value?.seated ?? []).filter((s) => s.userId === currentUser.value?.id).flatMap((s) => (seats[s.position] ? [seats[s.position]] : []))
})

// Who is being asked: any seat with choices. You answer for the first that is yours,
// the one you are looking at first.
const asked = computed(() => Object.entries(game.value?.question ?? {}).filter(([, q]) => q?.choices?.length).map(([id]) => id))
const viewing = ref<string | null>(null)
const allSeats = ref(false)
const answering = computed(() => {
  const yours = asked.value.filter((id) => mySeats.value.includes(id))
  return yours.find((id) => id === viewing.value) ?? yours[0] ?? null
})
const question = computed(() => (answering.value ? (game.value?.question[answering.value] ?? null) : null))
const choices = computed<ChoiceView[]>(() => question.value?.choices ?? [])
const waitingOn = computed(() => (answering.value ? null : (asked.value.map((id) => playerName.value[id]).find(Boolean) ?? null)))

// the seat shown: yours at first, then whoever of yours is asked
const viewed = computed(() => players.value.find((p) => p.id === viewing.value) ?? players.value.find((p) => mySeats.value.includes(p.id)) ?? players.value[0] ?? null)
watch(answering, (id) => {
  if (id) viewing.value = id
}, { immediate: true })
const shownSeats = computed(() => (allSeats.value ? players.value : viewed.value ? [viewed.value] : []))
const minionsOf = (player: PlayerView) => (game.value?.minions ?? []).filter((m) => m.engagedWith === player.id || player.engagedMinions?.includes(m.id))
const focused = computed<CardView[]>(() => game.value?.focusedCards ?? [])

// What the board draws, by id: a choice about anything else is a button instead.
const onBoard = computed(() => {
  const g = game.value
  const ids = new Set<string>()
  if (!g) return ids
  const add = (xs: { id: string }[] | null | undefined) => xs?.forEach((x) => ids.add(x.id))
  for (const v of g.villains ?? []) {
    ids.add(v.id)
    add(v.attachments)
    add(v.upgrades)
  }
  add(g.mainSchemes)
  add(g.sideSchemes)
  add(g.encounterDiscard)
  add(focused.value)
  for (const p of shownSeats.value) {
    ids.add(p.id)
    add(p.allies)
    p.allies?.forEach((a) => add(a.upgrades))
    add(p.supports)
    add(p.upgrades)
    add(p.discard)
    for (const m of minionsOf(p)) {
      ids.add(m.id)
      add(m.attachments)
      add(m.upgrades)
    }
  }
  add(viewed.value?.hand ?? [])
  // what is being paid for, and with, sits on the paying tray
  if (g.activeCost) {
    add(g.activeCost.card ? [g.activeCost.card] : [])
    add(g.activeCost.spent)
  }
  return ids
})
// everything with an id, for naming what a button is about
const things = computed(() => {
  const g = game.value
  const map: Record<string, { code: string; name: string }> = {}
  if (!g) return map
  const add = (xs: { id: string; code: string; name: string }[] | null | undefined) => xs?.forEach((x) => (map[x.id] = { code: x.code, name: x.name }))
  for (const p of g.players ?? []) {
    map[p.id] = { code: p.code, name: p.name }
    add(p.hand)
    add(p.discard)
    add(p.allies)
    p.allies?.forEach((a) => add(a.upgrades))
    add(p.supports)
    add(p.upgrades)
  }
  for (const v of g.villains ?? []) {
    add([v])
    add(v.attachments)
    add(v.upgrades)
  }
  for (const m of g.minions ?? []) {
    add([m])
    add(m.attachments)
    add(m.upgrades)
  }
  add(g.mainSchemes)
  add(g.sideSchemes)
  add(g.encounterDiscard)
  add(g.focusedCards)
  if (g.activeCost) {
    add(g.activeCost.card ? [g.activeCost.card] : [])
    add(g.activeCost.spent)
  }
  return map
})
const boardChoices = computed(() => choices.value.filter((c) => c.target && onBoard.value.has(c.target)))
// a player's choice is also a button, named for the player, as the first player is chosen
const buttons = computed(() => choices.value.filter((c) => !c.target || !onBoard.value.has(c.target) || c.kind === 'player'))

const choose = (index: number) => {
  if (answering.value && game.value) void answer(game.value.questionVersion, index, answering.value)
}
provideChoices(() => boardChoices.value, choose, () => question.value?.source ?? null)

const filingBug = ref(false)
const debugging = ref(false)
// on a narrower screen the log starts shut
const showSidebar = ref(!matchMedia('(width < 1100px)').matches)

const undoToEntry = (step: number, label: string) => {
  if (window.confirm(t('log.undoConfirm', { entry: label }))) void undoTo(step)
}

// Cmd/Ctrl+Z steps back one answer; space takes the only choice there is.
const onKeydown = (event: KeyboardEvent) => {
  if (spectate) return
  const typing = event.target instanceof HTMLInputElement || event.target instanceof HTMLTextAreaElement
  if (typing || document.querySelector('dialog[open]')) return
  if ((event.metaKey || event.ctrlKey) && !event.shiftKey && event.key.toLowerCase() === 'z' && canUndo.value) {
    event.preventDefault()
    void undo()
  }
  if (event.key === ' ' && !event.metaKey && !event.ctrlKey && !event.altKey && choices.value.length === 1) {
    event.preventDefault()
    choose(choices.value[0].index)
  }
}
onMounted(() => document.addEventListener('keydown', onKeydown))
onBeforeUnmount(() => document.removeEventListener('keydown', onKeydown))

const scenarioName = computed(() => game.value?.villains?.[0]?.name ?? null)
const active = computed(() => game.value?.activePlayer ?? null)
const resetGame = () => {
  if (window.confirm(t('game.bar.restartConfirm'))) void reset()
}
</script>

<template>
  <div class="game-view">
    <GameBar
      :can-undo="canUndo"
      :show-sidebar="showSidebar"
      :spectate="spectate"
      :guest="guest"
      :round="game?.round ?? null"
      :phase="game?.phase ?? null"
      :turn="active ? playerName[active] : null"
      :name="scenarioName"
      @reset="resetGame"
      @undo="undo($event)"
      @export="exportGame(gameId)"
      @file-bug="filingBug = true"
      @debug="debugging = true"
      @toggle-sidebar="showSidebar = !showSidebar"
    />
    <FileBug :open="filingBug" :game-id="gameId" @close="filingBug = false" />
    <DebugDialog :open="debugging" @close="debugging = false" @send="debug" />
    <CardOverlay />

    <div class="layout" :class="{ 'no-side': !showSidebar }">
      <main class="board">
        <Lobby v-if="data && !started && table?.withFriends" :table="table" @join="join" @leave="leave" @start="start" />
        <p v-else-if="!data" class="notice">{{ t('game.loading') }}</p>
        <p v-else-if="!game" class="notice">{{ t('game.notDealt') }}</p>

        <template v-if="game">
          <div class="half">
            <VillainArea :game="game" />

            <section v-if="focused.length" class="focused" :aria-label="t('game.zones.focused')">
              <span class="zone-label">{{ t('game.zones.focused') }}</span>
              <div class="cards">
                <BoardCard v-for="card in focused" :id="card.id" :key="card.id" :code="card.code" :name="card.name" :morph="false" />
              </div>
            </section>
          </div>

          <div class="player-half">
            <div class="half">
              <SeatTabs
                v-if="players.length > 1"
                :players="players"
                :viewing="viewed?.id ?? null"
                :asked="asked"
                :active="active"
                :mine="withFriends ? mySeats : []"
                :all="allSeats"
                @view="(id) => ((viewing = id), (allSeats = false))"
                @all="allSeats = !allSeats"
              />
              <PlayerArea
                v-for="seat in shownSeats"
                :key="seat.id"
                :player="seat"
                :minions="minionsOf(seat)"
                :active="seat.id === active && players.length > 1"
                :compact="allSeats && players.length > 1"
              >
                <template v-if="seat.id === viewed?.id">
                  <PayingTray v-if="game.activeCost" :cost="game.activeCost" :things="things" :player="playerName[game.activeCost.player] ?? null" />
                  <QuestionZone
                    :kind="question?.kind ?? null"
                    :prompt="question?.prompt ?? null"
                    :buttons="buttons"
                    :on-board="boardChoices.length"
                    :asking="answering && mySeats.length > 1 ? playerName[answering] : null"
                    :waiting-on="waitingOn"
                    :outcome="game.outcome"
                    :things="things"
                    @choose="choose"
                  />
                  <p v-if="error" class="error">
                    {{ tr(error) }}
                    <button v-if="!spectate" type="button" class="file-bug-link" @click="filingBug = true">{{ t('game.bar.fileBug') }}</button>
                  </p>
                </template>
              </PlayerArea>
            </div>
            <HandFan
              v-if="viewed"
              class="floating-hand"
              :cards="viewed.hand"
              :count="viewed.handCount"
              :label="players.length > 1 ? t('game.handOf', { name: viewed.name }) : t('game.hand')"
            />
          </div>
        </template>
      </main>

      <aside v-if="showSidebar" class="side">
        <GameLog
          :entries="log"
          :can-undo="!spectate && canUndo"
          :can-chat="!spectate"
          :has-older="hasOlder"
          :scenario="scenarioName"
          @undo="undoToEntry"
          @say="say"
          @load-older="loadOlder"
        />
      </aside>
    </div>
  </div>
</template>

<style scoped>
.game-view { display: flex; flex-direction: column; flex: 1; min-height: 0; }
.layout {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 320px;
  gap: 12px;
  flex: 1;
  min-height: 0;
  padding: 12px;
}
.layout.no-side { grid-template-columns: minmax(0, 1fr); }
.board {
  display: flex;
  flex-direction: column;
  gap: 12px;
  min-width: 0;
  min-height: 0;
  overflow: hidden auto;
  padding-right: 2px;
}
.board > * { flex: none; }
/* the villain's side fills the top half of the board, the players' the bottom */
.board > .half, .player-half > .half { flex: 1 1 0; display: flex; flex-direction: column; gap: 12px; min-height: 0; overflow: hidden auto; }
.half > * { flex: none; }
.half > :deep(.villain-area), .half > :deep(.player-area) { flex: 1 0 auto; }
/* the hand floats over the foot of the player area */
.board > .player-half { flex: 1 1 0; display: flex; flex-direction: column; min-height: 0; position: relative; }
.floating-hand { position: absolute; z-index: 0; inset: auto 0 0; pointer-events: none; }
.floating-hand :deep(.in-hand) { pointer-events: auto; }
.side { display: flex; flex-direction: column; gap: 10px; min-height: 0; }
.notice { margin: auto; color: var(--text-muted); }
.error { margin: 0; padding: 8px 10px; border-radius: 8px; border: 1px solid rgb(214 80 88 / 0.5); background: rgb(214 80 88 / 0.08); color: #f0a4a9; font-size: 0.8rem; overflow-wrap: anywhere; }
.file-bug-link { background: none; border: 0; padding: 0; color: inherit; font: inherit; text-decoration: underline; cursor: pointer; }

.focused {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px 16px;
  border-radius: 14px;
  background: radial-gradient(ellipse at 50% 0%, rgb(226 163 59 / 0.16), transparent 70%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.4);
  animation: reveal 0.3s ease-out;
}
.focused .cards { display: flex; flex-wrap: wrap; gap: 14px; justify-content: center; }
.zone-label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--amber); text-align: center; }
@keyframes reveal { from { opacity: 0; transform: translateY(-6px); } }

.side :deep(.game-log) { flex: 1; }

@media (width < 1100px) {
  .layout { grid-template-columns: minmax(0, 1fr); grid-template-rows: minmax(0, 1fr) auto; }
  .side { max-height: 45dvh; }
}
</style>
