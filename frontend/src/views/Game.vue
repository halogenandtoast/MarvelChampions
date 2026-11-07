<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { parseInput, useServerText } from '../i18n'
import { currentUser } from '../auth'
import { exportGame, fetchScenarios } from '../api'
import { useGame } from '../composables/useGame'
import { provideChoices } from '../composables/useChoices'
import type { AreaView, ChoiceView, PlayerView, ScenarioOption } from '../server'
import { useScenarios } from '../scenarios'
import AreaTabs from '../components/AreaTabs.vue'
import CardOverlay from '../components/CardOverlay.vue'
import DebugDialog from '../components/DebugDialog.vue'
import ConfirmDelete from '../components/ConfirmDelete.vue'
import FileBug from '../components/FileBug.vue'
import GameBar from '../components/GameBar.vue'
import CompetitionStatus from '../components/CompetitionStatus.vue'
import GameLog from '../components/GameLog.vue'
import HandFan from '../components/HandFan.vue'
import Lobby from '../components/Lobby.vue'
import PlayerArea from '../components/PlayerArea.vue'
import PayingTray from '../components/PayingTray.vue'
import QuestionZone from '../components/QuestionZone.vue'
import River, { type RiverAttacker, type RiverCard, type RiverScheme, type RiverSchemer } from '../components/River.vue'
import SeatTabs from '../components/SeatTabs.vue'
import SetupBar from '../components/SetupBar.vue'
import StoryReading from '../components/StoryReading.vue'
import CardPick from '../components/CardPick.vue'
import { searchedCards, searchCardIds } from '../composables/cardSearch'
import MissionArea from '../components/MissionArea.vue'
import VillainArea from '../components/VillainArea.vue'

const route = useRoute()
const { t } = useI18n()
const tr = useServerText()
const spectate = route.meta.spectate === true
const gameId = route.params.id as string
const { error, game, data, log, river, riverCleared, canceledEvent, activationAlert, activationBanner, activationKind, hasOlder, loadOlder, say, undoTo, started, table, answer, reset, undo, canUndo, debug, join, leave, start } =
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
const choices = computed<ChoiceView[]>(() => {
  const options = question.value?.choices ?? []
  if (question.value?.prompt !== '$label.revealBoostCards') return options
  const first = game.value?.boostCards?.find(card => !card.code)?.id
  return first ? options.map(choice => choice.label === '$label.revealBoostCards' ? { ...choice, target: first } : choice) : options
})
const waitingOn = computed(() => (answering.value ? null : (asked.value.map((id) => playerName.value[id]).find(Boolean) ?? null)))

// the seat shown: yours at first, then whoever of yours is asked
const viewed = computed(() => players.value.find((p) => p.id === viewing.value) ?? players.value.find((p) => mySeats.value.includes(p.id)) ?? players.value[0] ?? null)
watch(answering, (id) => {
  if (id) viewing.value = id
}, { immediate: true })
const shownSeats = computed(() => (allSeats.value ? players.value : viewed.value ? [viewed.value] : []))
const minionsOf = (player: PlayerView) => (game.value?.minions ?? []).filter((m) => !m.attachedToEnvironment && (m.engagedWith === player.id || player.engagedMinions?.includes(m.id)))
// the environments in a player's play area (Ebony Maw's Spells)
const environmentsOf = (player: PlayerView) => (game.value?.environments ?? []).filter((e) => e.owner === player.id)

// Game areas, when the players are split into several: the one of the seat you look at
// first, the others a tab away. One area is drawn as a game always was.
const areas = computed<AreaView[]>(() => game.value?.areas ?? [])
const areaOf = (playerId: string | undefined) => areas.value.find((a) => playerId !== undefined && a.players.includes(playerId)) ?? null
// yours: your first seat's; shown: the seat you look at's, until another is picked
const ownArea = computed(() => areaOf(mySeats.value[0])?.id ?? game.value?.viewerArea ?? areas.value[0]?.id ?? null)
const pickedArea = ref<number | null>(null)
watch(() => viewed.value?.id, () => (pickedArea.value = null))
const splitAreas = computed(() => areas.value.length > 1)
const shownArea = computed(() => (splitAreas.value ? (areas.value.find((a) => a.id === (pickedArea.value ?? areaOf(viewed.value?.id)?.id ?? ownArea.value)) ?? null) : null))
const orderedAreas = computed(() => [...areas.value].sort((a, b) => Number(b.id === ownArea.value) - Number(a.id === ownArea.value)))
// everything in an area, by id: its villains and schemes, its players and their cards
const idsIn = (area: AreaView) => {
  const g = game.value
  const ids = new Set<string>([...area.villains, ...area.mainSchemes, ...area.sideSchemes, ...area.environments, ...area.players])
  for (const v of g?.villains ?? []) if (ids.has(v.id)) [...(v.attachments ?? []), ...(v.upgrades ?? [])].forEach((x) => ids.add(x.id))
  for (const p of g?.players ?? []) {
    if (!ids.has(p.id)) continue
    ;[...(p.allies ?? []), ...(p.supports ?? []), ...(p.upgrades ?? []), ...(p.hand ?? []), ...(p.discard ?? [])].forEach((x) => ids.add(x.id))
    for (const m of minionsOf(p)) [m, ...(m.attachments ?? []), ...(m.upgrades ?? [])].forEach((x) => ids.add(x.id))
  }
  return ids
}
const litAreas = computed(() =>
  splitAreas.value ? areas.value.filter((a) => a.id !== shownArea.value?.id && choices.value.some((c) => c.target && idsIn(a).has(c.target))).map((a) => a.id) : [],
)
// showing an area shows one of its players too
const showArea = (id: number) => {
  pickedArea.value = id
  const area = areas.value.find((a) => a.id === id)
  if (area?.players.length && !area.players.includes(viewed.value?.id ?? '')) {
    viewing.value = area.players[0]
    allSeats.value = false
    pickedArea.value = id
  }
}
// what is happening now: cards revealed, boost cards dealt and turned up, and those the
// server has told of before the state that follows
const searchIds = computed(() => game.value ? searchCardIds(game.value) : new Set<string>())
const boostOrder = ref<string[]>([])
watch(() => [...new Set([
  ...(game.value?.boostCards ?? []).filter(card => card.code).map(card => card.id),
  ...(game.value?.boostCards ?? []).filter(card => !card.code).map(card => card.id),
  ...river.value.filter(card => card.boost).map(card => card.id),
])], ids => {
  boostOrder.value = [...boostOrder.value.filter(id => ids.includes(id)), ...ids.filter(id => !boostOrder.value.includes(id))]
}, { immediate: true })
const riverCards = computed<RiverCard[]>(() => {
  const cards = new Map<string, RiverCard>()
  if (!riverCleared.value) {
    for (const c of game.value?.focusedCards ?? []) if (!searchIds.value.has(c.id)) cards.set(c.id, c)
    for (const b of game.value?.boostCards ?? []) if (b.code) cards.set(b.id, b)
  }
  for (const c of river.value) cards.set(c.id, { ...cards.get(c.id), ...c })
  for (const id of game.value?.scheming ?? []) {
    const enemy = [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].find(enemy => enemy.id === id)
    if (enemy) cards.set(id, { id, code: enemy.code, name: enemy.name })
  }
  if (['$label.resolveEnemyAttack', '$label.resolveEnemyScheme'].includes(question.value?.prompt ?? '') && question.value?.source) {
    const id = question.value.source
    const enemy = things.value[id]
    if (enemy && id !== attacker.value?.id) cards.set(id, { id, ...enemy })
  }
  // Keep the source of an offered response or interrupt visible, including upgrades attached
  // to schemes. Its choice is answered on the card in the river.
  for (const choice of choices.value) {
    if (choice.kind !== 'ability' || !/Response|Interrupt/.test(choice.abilityType ?? '') || !choice.target) continue
    const card = things.value[choice.target]
    if (card) cards.set(choice.target, { id: choice.target, ...card })
  }
  if (Object.values(game.value?.question ?? {}).some(q => q?.prompt === '$label.revealBoostCards')) {
    for (const enemy of [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])]) {
      if (enemy.boostCards) cards.set(enemy.id, { id: enemy.id, code: enemy.code, name: enemy.name })
    }
  }
  if (!riverCleared.value && (attacker.value || [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].some(enemy => cards.has(enemy.id)))) {
    for (const boost of game.value?.boostCards ?? []) if (!boost.code && !cards.has(boost.id)) cards.set(boost.id, boost)
  }
  const rank = (card: RiverCard) => {
    const index = boostOrder.value.indexOf(card.id)
    return index < 0 ? -1 : index
  }
  return [...cards.values()].sort((a, b) => rank(a) - rank(b))
})
const riverEnemyIds = computed(() => {
  const ids = new Set(riverCards.value.map(card => card.id))
  if (attacker.value) ids.add(attacker.value.id)
  return [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].filter(enemy => ids.has(enemy.id)).map(enemy => enemy.id)
})
// a card in the river may be on the table or in hand too, as a revealed minion is
const elsewhere = computed(() => {
  const g = game.value
  const ids = new Set([...(g?.minions ?? []), ...(g?.sideSchemes ?? []), ...(g?.environments ?? [])].map((x) => x.id))
  for (const p of g?.players ?? []) [...(p.hand ?? []), ...(p.supports ?? []), ...(p.upgrades ?? []), ...(p.allies ?? []), ...(p.discard ?? [])].forEach((c) => ids.add(c.id))
  return ids
})
// the enemy attacking, as it stands now
const attacker = computed<RiverAttacker | null>(() => {
  const id = game.value?.attack?.attacker
  if (!id || riverCleared.value) return null
  return [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].find((e) => e.id === id) ?? null
})
const schemer = computed<RiverSchemer | null>(() => {
  if (attacker.value) return null
  const pending = Object.values(game.value?.question ?? {}).find(q => q?.prompt === '$label.resolveEnemyScheme')
  const id = pending?.source ?? game.value?.scheming?.[0]
  const enemy = [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].find(enemy => enemy.id === id)
    ?? (Object.values(game.value?.question ?? {}).some(q => q?.prompt === '$label.revealBoostCards')
      ? [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].find(enemy => enemy.boostCards > 0) : null)
  if (!enemy) return null
  const label = pending?.choices.find(choice => choice.label?.startsWith('$label.placeActivationThreat '))?.label
  const amount = label ? parseInput(label).params.count : null
  return { ...enemy, scheme: typeof amount === 'number' ? amount : enemy.scheme === null ? null : enemy.scheme + (enemy.activationBoost ?? 0) }
})
const riverLabel = computed(() => {
  if (choices.value.some((choice) => choice.kind === 'ability' && choice.abilityType?.includes('Interrupt'))) return t('game.river.interrupts')
  if (choices.value.some((choice) => choice.kind === 'ability' && choice.abilityType?.includes('Response'))) return t('game.river.responses')
  if (riverCleared.value) return null
  const attack = game.value?.attack
  if (attack) return t('game.river.attacks', { attacker: things.value[attack.attacker]?.name ?? '', target: things.value[attack.target]?.name ?? '' })
  if (question.value?.prompt === '$label.resolveEnemyScheme' && question.value.source) return t('game.river.schemes', { enemy: things.value[question.value.source]?.name ?? '' })
  const schemer = [...(game.value?.villains ?? []), ...(game.value?.minions ?? [])].find(enemy => game.value?.scheming?.includes(enemy.id)
    || (Object.values(game.value?.question ?? {}).some(q => q?.prompt === '$label.revealBoostCards') && enemy.boostCards > 0))
  if (schemer) return t('game.river.schemes', { enemy: schemer.name })
  const threat = game.value?.threat
  if (!threat || !riverScheme.value) return null
  if (threat.source === 'ThreatFromAcceleration') return t('game.river.placeThreat')
  return t('game.river.threat', { source: t(`game.river.${threat.source}`), amount: threat.amount, scheme: riverScheme.value.name })
})
// the scheme threat would be placed on, as it is and would be
const riverScheme = computed<RiverScheme | null>(() => {
  const threat = game.value?.threat
  if (!threat || riverCleared.value || game.value?.attack) return null
  const s = [...(game.value?.mainSchemes ?? []), ...(game.value?.sideSchemes ?? [])].find((x) => x.id === threat.scheme)
  return s ? { id: s.id, code: s.code, name: s.name, from: s.threat, amount: threat.amount, threshold: threat.threshold ?? null } : null
})

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
  for (const e of g.environments ?? []) {
    if (e.owner && !shownSeats.value.some((p) => p.id === e.owner)) continue
    ids.add(e.id)
    add(e.attachments)
  }
  add(g.encounterDiscard)
  add(g.victoryDisplay ?? [])
  add(g.collection ?? [])
  add(riverCards.value)
  for (const p of shownSeats.value) {
    ids.add(`dealt-${p.id}`)
    ids.add(p.id)
    add(p.allies)
    p.allies?.forEach((a) => (add(a.upgrades), add(a.attachments)))
    add(p.supports)
    add(p.upgrades)
    add(p.attachments)
    add(p.obligations)
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
    p.allies?.forEach((a) => (add(a.upgrades), add(a.attachments)))
    add(p.supports)
    add(p.upgrades)
    add(p.attachments)
    add(p.obligations)
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
  for (const scheme of [...(g.mainSchemes ?? []), ...(g.sideSchemes ?? [])]) {
    add(scheme.upgrades)
    add(scheme.attachments)
    add(scheme.attachedCards)
  }
  for (const environment of g.environments ?? []) {
    add([environment])
    add(environment.attachments)
  }
  add(g.encounterDiscard)
  add(g.victoryDisplay ?? [])
  add(g.collection ?? [])
  add(g.focusedCards)
  if (g.activeCost) {
    add(g.activeCost.card ? [g.activeCost.card] : [])
    add(g.activeCost.spent)
  }
  return map
})
const boardChoices = computed(() => choices.value.filter((c) => c.target && onBoard.value.has(c.target)))
// a player's choice is also a button, named for the player, as the first player is chosen
const riverActions = computed(() => choices.value.filter(c => /^\$label\.(applyAttackDamage|placeActivationThreat)\b/.test(c.label ?? '')))
const buttons = computed(() => choices.value.filter((c) => !riverActions.value.includes(c) && c.kind !== 'declareWild' && (!c.target || !onBoard.value.has(c.target) || c.kind === 'player')))
// a wild paid is declared as a type on its icon in the paying tray
const declares = computed(() => choices.value.filter((c) => c.kind === 'declareWild'))

const choose = (index: number) => {
  if (answering.value && game.value) void answer(game.value.questionVersion, index, answering.value)
}
// a campaign scenario's story and rules, read by everyone before its player goes on
const reading = computed(() => Object.entries(game.value?.question ?? {}).find(([, q]) => q?.reading?.length) ?? null)
// a question answered by picking a card in no play area, as a campaign's Tech upgrade
const picking = computed(() => Object.entries(game.value?.question ?? {}).find(([, q]) => q && (searchedCards(game.value, q).length || q.choices.some((c) => c.kind === 'card'))) ?? null)
// either pops out over the board; closed, the question box opens it again
const closedAt = ref<number | null>(null)
const popOut = computed(() => (reading.value || picking.value ? (closedAt.value === game.value?.questionVersion ? 'closed' : 'open') : null))

provideChoices(() => boardChoices.value, choose, () => question.value?.prompt === '$label.revealBoostCards'
  ? choices.value.find(choice => choice.label === '$label.revealBoostCards')?.target ?? null
  : question.value?.source ?? null, () => question.value?.assignedDamage ?? {})

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

// the lobby says what is to be played, named as the new game page named it
const scenarioList = ref<ScenarioOption[]>([])
const chosen = useScenarios(scenarioList)
const inLobby = computed(() => !!data.value && !started.value && !!table.value?.withFriends)
watch(inLobby, async (lobby) => {
  if (lobby && !scenarioList.value.length) scenarioList.value = (await fetchScenarios().catch(() => null))?.scenarios ?? []
}, { immediate: true })
onBeforeUnmount(() => document.removeEventListener('keydown', onKeydown))

// the scenario's own villain, the first of the first area's; at a split table, the
// villain of the area shown, when it has one
const scenarioName = computed(() => {
  const first = shownArea.value?.activeVillain ?? game.value?.areas?.[0]?.villains?.[0]
  return (game.value?.villains ?? []).find((v) => v.id === first)?.name ?? game.value?.villains?.[0]?.name ?? null
})
const active = computed(() => game.value?.activePlayer ?? null)
const restartDialog = ref<InstanceType<typeof ConfirmDelete> | null>(null)
const resetGame = () => restartDialog.value?.open()
</script>

<template>
  <div class="game-view">
    <ConfirmDelete ref="restartDialog" :title="t('game.bar.restartScenario')" :confirm-label="t('game.bar.restartScenario')" @confirm="reset()">
      <p>{{ t('game.bar.restartConfirm') }}</p>
    </ConfirmDelete>
    <GameBar
      :can-undo="canUndo"
      :show-sidebar="showSidebar"
      :spectate="spectate"
      :guest="guest"
      :round="game?.round ?? null"
      :phase="game?.competition?.phase ?? game?.phase ?? null"
      :turn="active ? playerName[active] : null"
      :name="scenarioName"
      :campaign="game?.campaign ?? null"
      :campaign-id="table?.campaign ?? null"
      @reset="resetGame"
      @undo="undo($event)"
      @export="exportGame(gameId)"
      @file-bug="filingBug = true"
      @debug="debugging = true"
      @toggle-sidebar="showSidebar = !showSidebar"
    />
    <CompetitionStatus v-if="game?.competition" :competition="game.competition" :shown-area="shownArea?.id ?? null" @show="showArea" />
    <p v-if="game?.competition?.replyPending && !game.outcome" class="competitive-reply" role="status">{{ t('civilWar.replyPending') }}</p>
    <StoryReading
      v-if="reading && popOut === 'open'"
      :sections="reading[1].reading ?? []"
      :can-continue="answering === reading[0]"
      :waiting-on="playerName[reading[0]] ?? null"
      @continue="choose(0)"
      @close="closedAt = game?.questionVersion ?? null"
    />
    <CardPick
      v-else-if="picking && popOut === 'open'"
      :prompt="picking[1].prompt ?? null"
      :choices="picking[1].choices"
      :searched="searchedCards(game, picking[1])"
      :can-choose="answering === picking[0]"
      :waiting-on="playerName[picking[0]] ?? null"
      @choose="choose"
      @close="closedAt = game?.questionVersion ?? null"
    />
    <FileBug :open="filingBug" :game-id="gameId" @close="filingBug = false" />
    <DebugDialog :open="debugging" @close="debugging = false" @send="debug" />
    <CardOverlay />

    <div class="layout" :class="{ 'no-side': !showSidebar }">
      <main class="board" :class="{ waiting: inLobby }">
        <Lobby v-if="inLobby && table" :table="table" @join="join" @leave="leave" @start="start">
          <SetupBar
            v-if="data?.setup"
            :portraits="table.competition ? [table.competition.registration.leader, table.competition.resistance.leader] : chosen.portraits(data.setup.scenarioCode)"
            :name="table.competition ? t('civilWar.title') : chosen.scenarioName(data.setup.scenarioCode)"
            :details="table.competition ? [t('difficulty.' + data.setup.difficulty), t('newGame.playerCount', table.seats)] : chosen.details(data.setup).concat(t('newGame.playerCount', table.seats))"
          />
        </Lobby>
        <p v-else-if="!data" class="notice">{{ t('game.loading') }}</p>
        <p v-else-if="!game" class="notice">{{ t('game.notDealt') }}</p>

        <template v-if="game">
          <div class="half villain-half">
            <AreaTabs :labels="game.competition ? Object.fromEntries(game.competition.teams.map((team) => [team.area, t(`civilWar.${team.team.toLowerCase()}`)])) : undefined" v-if="splitAreas" :areas="orderedAreas" :numbers="Object.fromEntries(areas.map((a, n) => [a.id, n + 1]))" :shown="shownArea?.id ?? null" :own="ownArea" :lit="litAreas" :names="playerName" @show="showArea" />
            <VillainArea :game="game" :area="shownArea" :river-enemy-ids="riverEnemyIds" />
            <MissionArea v-if="game.mission" :mission="game.mission" />
          </div>

          <River class="river" :actions="riverActions" @choose="choose" :activation-alert="activationAlert" :activation-banner="activationBanner" :activation-kind="activationKind" :cards="riverCards" :reaction="choices.some(choice => choice.kind === 'ability' && /Response|Interrupt/.test(choice.abilityType ?? ''))" :canceled-event="canceledEvent" :label="riverLabel" :attacker="attacker" :schemer="schemer" :scheme="riverScheme" :elsewhere="elsewhere" :shared="!!game.activeCost">
            <PayingTray v-if="game.activeCost" :cost="game.activeCost" :things="things" :player="playerName[game.activeCost.player] ?? null" :elsewhere="elsewhere" :declares="declares" @choose="choose" />
          </River>

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
                :river-enemy-ids="riverEnemyIds"
                v-for="seat in shownSeats"
                :key="seat.id"
                :player="seat"
                :minions="minionsOf(seat)"
                :environments="environmentsOf(seat)"
                :active="seat.id === active && players.length > 1"
                :compact="allSeats && players.length > 1"
              >
                <template v-if="seat.id === viewed?.id">
                  <QuestionZone
                    :kind="question?.kind ?? null"
                    :prompt="reading && popOut ? '$game.prompt.read' : (question?.prompt ?? null)"
                    :buttons="buttons"
                    :on-board="boardChoices.length"
                    :asking="answering && mySeats.length > 1 ? playerName[answering] : null"
                    :waiting-on="waitingOn"
                    :outcome="game.outcome"
                    :things="things"
                    :pop-out="popOut"
                    @choose="choose"
                    @reopen="closedAt = null"
                  />
                  <RouterLink v-if="game.outcome && table?.campaign" :to="`/campaigns/${table.campaign}`" class="btn primary big">{{ t('game.outcome.toCampaign') }}</RouterLink>
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
/* the villain's side and the river a quarter of the board each, at least a card high, the
   players' side the rest */
.board {
  --row: calc(var(--card-width) / var(--card-aspect) + 14px);
  display: grid;
  grid-template-rows: minmax(min-content, 25%) minmax(var(--row), 25%) minmax(0, 1fr);
  gap: 12px;
  min-width: 0;
  min-height: 0;
  overflow: hidden auto;
  padding-right: 2px;
}
.board > * { flex: none; }
.board > .notice { grid-row: 1 / -1; }
/* the lobby is as tall as it needs, the board scrolling it */
.board.waiting { display: block; }
.board > .half, .player-half > .half { flex: 1 1 0; display: flex; flex-direction: column; gap: 12px; min-height: 0; overflow: hidden auto; }
.half > * { flex: none; }
.half > :deep(.villain-area), .half > :deep(.player-area) { flex: 1 0 auto; }
/* the hand floats over the foot of the player area */
.board > .player-half { display: flex; flex-direction: column; min-height: 0; position: relative; }
.floating-hand { position: absolute; z-index: 0; inset: auto 0 0; pointer-events: none; }
.floating-hand :deep(.in-hand) { pointer-events: auto; }
/* beneath what you are asked, but a card pointed at comes in front of it */
.floating-hand:hover, .floating-hand:focus-within { z-index: 2; }
.side { display: flex; flex-direction: column; gap: 10px; min-height: 0; }
.notice { margin: auto; color: var(--text-muted); }
.error { margin: 0; padding: 8px 10px; border-radius: 8px; border: 1px solid rgb(214 80 88 / 0.5); background: rgb(214 80 88 / 0.08); color: #f0a4a9; font-size: 0.8rem; overflow-wrap: anywhere; }
.file-bug-link { background: none; border: 0; padding: 0; color: inherit; font: inherit; text-decoration: underline; cursor: pointer; }


.side :deep(.game-log) { flex: 1; }

@media (width < 1100px) {
  .layout { grid-template-columns: minmax(0, 1fr); grid-template-rows: minmax(0, 1fr) auto; }
  .side { max-height: 45dvh; }
}
</style>
