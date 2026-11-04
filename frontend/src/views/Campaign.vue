<script lang="ts" setup>
import { computed, onMounted, ref, useTemplateRef } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { BookOpen, Play, Trash2 } from 'lucide-vue-next'
import { buyMarketCard, changeCampaignDeck, continueCampaign, deleteCampaign, fetchCampaign, fetchStarterDecks, joinCampaign, leaveCampaign, startCampaign, type DeckChoice } from '../api'
import { currentUser } from '../auth'
import { fetchDecks } from '../browser/lib/api'
import type { DeckList, DeckSummary } from '../browser/types/deck'
import { useCards } from '../cards'
import { campaignGuides, campaignGuideSections } from '../campaigns'
import { useScenarios } from '../scenarios'
import DeckPicker from '../components/DeckPicker.vue'
import Lobby from '../components/Lobby.vue'
import StoryReading from '../components/StoryReading.vue'
import ScenarioCard from '../components/ScenarioCard.vue'
import { toDeckChoice, type DeckFace, type DeckKey } from '../deckChoice'
import { useServerText } from '../i18n'
import type { CampaignView, StarterDeck, TitanMark } from '../server'

// A campaign's overview: its log as the printed one has it, the scenarios won and the
// one next, and the button that deals it (or goes back to the game of it being played).
const { t } = useI18n()
const tr = useServerText()
const route = useRoute()
const router = useRouter()
const cards = useCards()

const view = ref<CampaignView | null>(null)
const error = ref<string | null>(null)
const working = ref(false)
const guideOpen = ref(false)
const id = String(route.params.id)

const load = () =>
  fetchCampaign(id)
    .then((v) => (view.value = v))
    .catch(() => (error.value = t('campaign.notFound')))
void load()

// a campaign with friends gathers its players as a game's lobby does; the last seat
// taken, or the owner beginning with those taken, deals its first game
const attempt = async (step: () => Promise<{ gameId?: string } | void>) => {
  working.value = true
  error.value = null
  try {
    const result = await step()
    if (result && result.gameId) return void (await router.push(`/games/${result.gameId}`))
    await load()
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('campaign.couldNotContinue')
    await load()
  }
  working.value = false
}
const join = (deck: DeckChoice) => attempt(() => joinCampaign(id, deck))
const leave = () => attempt(() => leaveCampaign(id))
const begin = () => attempt(() => startCampaign(id))

// the seat each column of the log is played from, and whether it is yours
const seatOf = (i: number) => view.value?.table.seated[i] ?? null
const yours = (i: number) => seatOf(i)?.userId === currentUser.value?.id
const short = (i: number) => view.value?.deckShort[i] ?? 0

// between games, a player may change their deck, keeping their hero (rulebook p.3)
const decks = ref<DeckSummary[]>([])
const starters = ref<StarterDeck[]>([])
onMounted(async () => {
  starters.value = await fetchStarterDecks().catch(() => [])
  decks.value = await fetchDecks().catch(() => [])
})
const dialog = useTemplateRef<HTMLDialogElement>('dialog')
const changing = ref<number | null>(null)
const picking = ref<DeckKey>('starter:spider-man')
const loaded = ref<{ face: DeckFace; list: DeckList } | null>(null)
const pickingLoad = ref(false)
const changeDeck = (i: number) => {
  changing.value = i
  loaded.value = null
  dialog.value?.showModal()
}
const useDeck = async () => {
  const seat = changing.value === null ? null : seatOf(changing.value)
  if (!seat) return
  const deck: DeckChoice = picking.value === 'loaded' && loaded.value ? { list: loaded.value.list } : toDeckChoice(picking.value)
  dialog.value?.close()
  await attempt(() => changeCampaignDeck(id, seat.position, deck))
}

const log = computed(() => view.value?.campaign.campaignLog)
const galaxy = computed(() => view.value?.campaign.kind === 'GalaxysMostWanted')
const titan = computed(() => view.value?.campaign.kind === 'MadTitansShadow')
const fearNoEvil = computed(() => view.value?.campaign.kind === 'FearNoEvilCampaign')
const fneScenarios = ['60134a', '60128a', '60121a', '60142a', '60151a']
const fneStatus = (code: string) => {
  const entry = log.value?.fearNoEvil?.resolved.find(([scenario]) => scenario === code)
  return entry ? t(entry[1] ? 'campaign.fneCompleted' : 'campaign.fneFailed') : t('campaign.fneUnresolved')
}
const shadowland = computed(() => view.value?.campaign.kind === 'ShadowlandCampaign')
const shield = computed(() => view.value?.campaign.kind === 'AgentsOfShieldCampaign')
const apocalypse = computed(() => view.value?.campaign.kind === 'AgeOfApocalypseCampaign')
const nextEvolution = computed(() => view.value?.campaign.kind === 'NextEvolution')
const mutant = computed(() => view.value?.campaign.kind === 'MutantGenesis')
const mutantPlayer = (identity: string) => log.value?.mutantGenesis?.mutantPlayers.find((p) => p.identity === identity) ?? null
const mutantSchemes = ['32171a', '32172a', '32173a', '32174a']
const sinister = computed(() => view.value?.campaign.kind === 'SinisterMotives')
const sinisterPlayer = (identity: string) => log.value?.sinister?.sinisterPlayers.find((p) => p.identity === identity) ?? null
// The Mad Titan's Shadow's log: its checkboxes by the scenario that checks them (p.28)
const titanMarks: [number, TitanMark[]][] = [
  [1, ['CosmoInPool', 'SecurityBreachInPool']],
  [2, ['ShawarmaInPool', 'BlackSwanInPool', 'AvengersTowerDamaged']],
  [3, ['SystemShockInPool', 'InfinityStones1BCompleted']],
  [4, ['NornStoneInPool', 'OdinInPool']],
]
const marked = (m: TitanMark) => (log.value?.titan?.marks ?? []).includes(m)
const expert = computed(() => view.value?.campaign.mode === 'ExpertCampaign')
const galaxyPlayer = (identity: string) => log.value?.galaxy.galaxyPlayers.find((p) => p.identity === identity) ?? null
// between games, a player spends their units on The Market's cards (rulebook p.5)
const between = computed(() => !!view.value && !view.value.currentGame && !view.value.waitingForPlayers && !!view.value.next)
const shopping = ref<number | null>(null)
const affordable = (identity: string) => (view.value?.market ?? []).filter(([, cost]) => cost <= (galaxyPlayer(identity)?.units ?? 0))
const buy = async (i: number, card: string) => {
  const seat = seatOf(i)
  if (!seat) return
  await attempt(() => buyMarketCard(id, seat.position, card).then(() => undefined))
}
// said once: the campaign's own title only when its players named it something else
const details = computed(() => {
  const v = view.value
  if (!v) return []
  const title = t(`campaign.${v.campaign.kind ?? 'RiseOfRedSkull'}`)
  return [...(v.name === title ? [] : [title]), t(`campaign.${v.campaign.mode}`), t(`difficulty.${v.difficulty}`), t('newGame.playerCount', v.table.seats)]
})
const name = (code: string | null | undefined) => (code ? cards.name(code, code) : t('campaign.none'))
const names = (codes: string[]) => (codes.length ? codes.map((c) => name(c)).join(', ') : t('campaign.none'))
const art = (code: string) => cards.sources(code).map((s) => `url('${s}')`).join(', ')
const campaignScenarios = computed(() => (view.value?.campaign.scenarios ?? []).map(code => ({ code, name: '', modulars: [] })))
const { tileName: scenarioTitle } = useScenarios(campaignScenarios)
const stage = (i: number) => {
  const c = view.value?.campaign
  if (!c) return 'later'
  if (i < c.completed) return 'won'
  if (i === c.completed && !c.lost) return 'current'
  return c.lost && i === c.completed ? 'lost' : 'later'
}
// a game lost is played again; the next scenario is dealt once the last is won
const action = computed(() => {
  const v = view.value
  if (!v || !v.next) return null
  if (v.currentGame) return t('campaign.resume')
  if (fearNoEvil.value && !v.campaign.campaignLog.fearNoEvil?.selectedScenario) return t('campaign.fneChooseNext')
  return t('campaign.continue', { villain: name(v.next) })
})

const play = async () => {
  working.value = true
  error.value = null
  try {
    const { gameId } = await continueCampaign(id)
    await router.push(`/games/${gameId}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('campaign.couldNotContinue')
    working.value = false
  }
}

const remove = async () => {
  if (!window.confirm(t('campaign.confirmDelete'))) return
  await deleteCampaign(id)
  await router.push('/')
}
</script>

<template>
  <div class="page-container">
    <div v-if="view && log" class="page-content campaign">
      <header class="head">
        <div>
          <p class="eyebrow">{{ details.join(' · ') }}</p>
          <h1 class="page-title">{{ view.name }}</h1>
        </div>
        <button type="button" class="btn ghost" @click="guideOpen = true"><BookOpen aria-hidden="true" />{{ $t('campaign.readGuide') }}</button>
        <button v-if="view.table.owner === currentUser?.id" type="button" class="btn ghost" :title="$t('campaign.delete')" @click="remove"><Trash2 aria-hidden="true" />{{ $t('campaign.delete') }}</button>
      </header>

      <ol v-if="!fearNoEvil" class="track">
        <li v-for="(code, i) in view.campaign.scenarios" :key="`${code}-${i}`" class="stop" :class="stage(i)">
          <ScenarioCard :code="code" :title="scenarioTitle(code)" :subtitle="$t('campaign.scenario', { n: i + 1 })" :selected="stage(i) === 'current'" :completed="stage(i) === 'won'" :lost="stage(i) === 'lost'" />
        </li>
      </ol>

      <section v-if="fearNoEvil && log.fearNoEvil" class="box">
        <h2>{{ $t('campaign.fneCrimeWave') }}</h2>
        <p>{{ $t('campaign.fneProgressionHelp') }}</p>
        <table class="fne-progress">
          <thead><tr><th>{{ $t('campaign.fneScenario') }}</th><th>{{ $t('campaign.fneProgress') }}</th><th>{{ $t('campaign.fneVillain') }}</th><th>{{ $t('campaign.fneResult') }}</th></tr></thead>
          <tbody><tr v-for="code in fneScenarios" :key="code">
            <td>{{ name(code) }}</td>
            <td>{{ '×'.repeat(log.fearNoEvil.progress.find(([scenario]) => scenario === code)?.[1] ?? 0) || '—' }}</td>
            <td>{{ log.fearNoEvil.underlings.find(([scenario]) => scenario === code)?.[1] ? name(log.fearNoEvil.underlings.find(([scenario]) => scenario === code)![1]) : '—' }}</td>
            <td>{{ fneStatus(code) }}</td>
          </tr></tbody>
        </table>
      </section>

      <Lobby v-if="view.waitingForPlayers" :table="view.table" @join="join" @leave="leave" @start="begin" />

      <div v-else class="next box">
        <p class="progress">{{ $t('campaign.progress', { done: fearNoEvil ? (log.fearNoEvil?.resolved.length ?? 0) + (log.fearNoEvil?.victorious ? 1 : 0) : view.campaign.completed, total: fearNoEvil ? 6 : view.campaign.scenarios.length }) }}</p>
        <p v-if="view.won" class="result good">{{ $t(mutant ? 'campaign.mutantWon' : galaxy ? 'campaign.galaxyWon' : titan ? 'campaign.titanWon' : sinister ? 'campaign.sinisterWon' : 'campaign.won') }}</p>
        <p v-else-if="view.campaign.lost" class="result bad">{{ $t(mutant ? 'campaign.mutantLost' : galaxy ? 'campaign.galaxyLost' : titan ? 'campaign.titanLost' : sinister ? 'campaign.sinisterLost' : 'campaign.lost') }}</p>
        <button v-if="action" type="button" class="btn primary big" :disabled="working" @click="play"><Play aria-hidden="true" />{{ action }}</button>
      </div>
      <p v-if="error" class="error">{{ tr(error) }}</p>

      <section class="box">
        <h2>{{ $t('campaign.playerInformation') }}</h2>
        <div class="players">
          <article v-for="(p, i) in log.players" :key="p.identity" class="player">
            <header>
              <span class="portrait" :style="{ backgroundImage: art(p.identity) }" />
              <div>
                <small>{{ $t('campaign.player', { n: p.number }) }}<template v-if="seatOf(i)"> · {{ $t('campaign.seatedBy', { name: seatOf(i)!.username }) }}</template></small>
                <strong>{{ name(p.identity) }}</strong>
              </div>
            </header>
            <dl v-if="galaxy">
              <dt v-if="expert">{{ $t('campaign.hitPoints') }}</dt>
              <dd v-if="expert">{{ p.hitPoints ?? $t('campaign.none') }}</dd>
              <dt>{{ $t('campaign.units') }}</dt>
              <dd>{{ galaxyPlayer(p.identity)?.units ?? 0 }}</dd>
              <dt>{{ $t('campaign.marketCards') }}</dt>
              <dd>{{ names(galaxyPlayer(p.identity)?.marketCards ?? []) }}</dd>
              <dt>{{ $t('campaign.collection') }}</dt>
              <dd>{{ names(galaxyPlayer(p.identity)?.collection ?? []) }}</dd>
            </dl>
            <dl v-else-if="mutant">
              <dt>{{ $t('campaign.role') }}</dt>
              <dd>{{ mutantPlayer(p.identity)?.role ? $t(`campaign.roles.${mutantPlayer(p.identity)!.role}`) : $t('campaign.none') }}</dd>
              <dt v-if="expert">{{ $t('campaign.hitPoints') }}</dt>
              <dd v-if="expert">{{ p.hitPoints ?? $t('campaign.none') }}</dd>
            </dl>
            <dl v-else-if="sinister">
              <dt v-if="expert">{{ $t('campaign.hitPoints') }}</dt>
              <dd v-if="expert">{{ p.hitPoints ?? $t('campaign.none') }}</dd>
              <dt>{{ $t('campaign.shieldTech') }}</dt>
              <dd>{{ name(sinisterPlayer(p.identity)?.shieldTech) }}</dd>
              <dt>{{ $t('campaign.aspectAdvantage') }}</dt>
              <dd>{{ name(sinisterPlayer(p.identity)?.aspectAdvantage?.[0]) }}<template v-if="sinisterPlayer(p.identity)?.aspectAdvantage"> ×{{ sinisterPlayer(p.identity)!.aspectAdvantage![1] }}</template></dd>
              <dt>{{ $t('campaign.planningAhead') }}</dt>
              <dd>{{ name(sinisterPlayer(p.identity)?.planningAhead) }}</dd>
            </dl>
            <dl v-else-if="titan && expert">
              <dt>{{ $t('campaign.hitPoints') }}</dt>
              <dd>{{ p.hitPoints ?? $t('campaign.none') }}</dd>
            </dl>
            <dl v-else-if="shadowland">
              <dt v-if="expert">{{ $t('campaign.hitPoints') }}</dt>
              <dd v-if="expert">{{ p.hitPoints ?? $t('campaign.none') }}</dd>
              <dt>{{ $t('campaign.shadowlandPrepared') }}</dt>
              <dd>{{ names(log.shadowland.preparedCards.find(([hero]) => hero === p.identity)?.[1] ?? []) }}</dd>
              <dt>{{ $t('campaign.shadowlandCorruption') }}</dt>
              <dd>{{ log.shadowland.overpoweringHistory.find(([hero]) => hero === p.identity)?.[1] ?? 0 }}</dd>
            </dl>
            <dl v-else-if="!titan">
              <dt v-if="view.campaign.mode === 'ExpertCampaign'">{{ $t('campaign.hitPoints') }}</dt>
              <dd v-if="view.campaign.mode === 'ExpertCampaign'">{{ p.hitPoints ?? $t('campaign.none') }}</dd>
              <dt v-if="view.campaign.mode === 'ExpertCampaign'">{{ $t('campaign.obligations') }}</dt>
              <dd v-if="view.campaign.mode === 'ExpertCampaign'">{{ names(p.obligations) }}</dd>
              <dt>{{ $t('campaign.techUpgrade') }}</dt>
              <dd>{{ name(p.techUpgrade) }}</dd>
              <dt>{{ $t('campaign.basicUpgrade') }}</dt>
              <dd>{{ name(p.conditionUpgrade) }}</dd>
              <dt>{{ $t('campaign.rescuedAllies') }}</dt>
              <dd>{{ names(p.rescuedAllies) }}</dd>
            </dl>
            <p v-if="short(i) > 0" class="short">{{ $t('campaign.deckShort', short(i)) }}</p>
            <button v-if="yours(i) && between" type="button" class="btn ghost" @click="changeDeck(i)">{{ $t('campaign.changeDeck') }}</button>
            <template v-if="galaxy && yours(i) && between">
              <button type="button" class="btn ghost" :aria-expanded="shopping === i" @click="shopping = shopping === i ? null : i">{{ $t('campaign.theMarket') }}</button>
              <ul v-if="shopping === i" class="market">
                <li v-for="[card, cost] in affordable(p.identity)" :key="card">
                  <button type="button" class="btn ghost" :disabled="working" @click="buy(i, card)">{{ $t('campaign.buy', { card: name(card), count: cost }, cost) }}</button>
                </li>
                <li v-if="!affordable(p.identity).length" class="hint">{{ $t('campaign.nothingAffordable') }}</li>
              </ul>
            </template>
            <p v-if="p.pending.length" class="pending">{{ $t('campaign.toChoose') }}: {{ p.pending.map((c) => $t(`campaign.${c}`)).join(', ') }}</p>
          </article>
        </div>
      </section>

      <section class="box">
        <h2>{{ $t('campaign.log') }}</h2>
        <dl v-if="fearNoEvil && log.fearNoEvil" class="scenario-log">
          <dt>{{ $t('campaign.fneTrust') }}</dt><dd>{{ $t(log.fearNoEvil.trustEstablished ? 'campaign.fneYes' : 'campaign.fneNo') }}</dd>
          <dt>{{ $t('campaign.fneMaryDefeated') }}</dt><dd>{{ $t(log.fearNoEvil.maryDefeated ? 'campaign.fneYes' : 'campaign.fneNo') }}</dd>
          <dt>{{ $t('campaign.fneRemoved') }}</dt><dd>{{ log.fearNoEvil.removedTitles.join(', ') || $t('campaign.none') }}</dd>
        </dl>
        <dl v-else-if="galaxy" class="scenario-log">
          <dt>{{ $t('campaign.scenario', { n: 3 }) }} · {{ $t('campaign.galacticArtifacts') }}</dt>
          <dd>{{ names(log.galaxy.galacticArtifacts) }}</dd>
          <dt>{{ $t('campaign.scenario', { n: 4 }) }} · {{ $t('campaign.powerStoneControl') }}</dt>
          <dd>{{ name(log.galaxy.powerStoneControl) }}</dd>
          <dt>{{ $t('campaign.scenario', { n: 4 }) }} · {{ $t('campaign.evasionCounters') }}</dt>
          <dd>{{ log.galaxy.evasionCounters ?? $t('campaign.none') }}</dd>
          <dt>{{ $t('campaign.headhunterDefeated') }}</dt>
          <dd>{{ [1, 2, 3, 4].map((n) => $t('campaign.scenario', { n }) + (log!.galaxy.headhunterDefeated.includes(n) ? ' ✓' : ' —')).join(', ') }}</dd>
        </dl>
        <dl v-else-if="shadowland" class="scenario-log">
          <dt>{{ $t('campaign.shadowlandProteges') }}</dt>
          <dd>{{ names(log.shadowland.removedProteges) }}</dd>
        </dl>
        <dl v-else-if="shield && log.shield" class="scenario-log">
          <dt>{{ $t('campaign.shieldEvidence') }}</dt><dd>{{ names(log.shield.earnedEvidence) }}</dd>
          <template v-for="[code, count] in log.shield.boardSecrets" :key="code"><dt>{{ name(code) }}</dt><dd>{{ count }}</dd></template>
          <dt>{{ $t('campaign.shieldBetrayed') }}</dt><dd>{{ names(log.shield.betrayedBoard) }}</dd>
          <dt>{{ $t('campaign.shieldCaptives') }}</dt><dd>{{ log.shield.rescuedCaptives }}</dd>
          <dt>{{ $t('campaign.shieldAdaptoids') }}</dt><dd>{{ names(log.shield.adaptoidEnvironments) }}</dd>
          <dt>{{ $t('campaign.shieldThunderbolts') }}</dt><dd>{{ names(log.shield.survivingThunderbolts) }}</dd>
        </dl>
        <dl v-else-if="apocalypse && log.apocalypse" class="scenario-log">
          <dt>{{ $t('campaign.aoaMissions') }}</dt>
          <dd>{{ names(log.apocalypse.missions.map(([, code]) => code)) }}</dd>
          <dt>{{ $t('campaign.aoaSuccesses') }}</dt>
          <dd>{{ names(log.apocalypse.successfulMissions) }}</dd>
          <dt>{{ $t('campaign.aoaOverseers') }}</dt>
          <dd>{{ names(log.apocalypse.defeatedOverseers) }}</dd>
          <template v-for="[hero, rewards] in log.apocalypse.apocalypseRewards" :key="hero">
            <dt>{{ name(hero) }} · {{ $t('campaign.aoaRewards') }}</dt>
            <dd>{{ names(rewards) }}</dd>
          </template>
        </dl>
        <dl v-else-if="nextEvolution && log.nextEvolution" class="scenario-log">
          <template v-for="[n, code] in log.nextEvolution.chosenSchemes" :key="n">
            <dt>{{ $t('campaign.scenario', { n }) }} · {{ $t('campaign.chosenSideScheme') }}</dt>
            <dd>{{ name(code) }}</dd>
          </template>
          <dt>{{ $t('campaign.earnedEnvironments') }}</dt>
          <dd>{{ names(log.nextEvolution.earnedEnvironments) }}</dd>
          <dt>{{ $t('campaign.maraudersDefeated') }}</dt>
          <dd>{{ names(log.nextEvolution.maraudersDefeated) }}</dd>
          <dt>{{ $t('campaign.morlocksSaved') }}</dt>
          <dd>{{ log.nextEvolution.morlocksSaved }}</dd>
          <template v-for="[n, damage] in log.nextEvolution.hopeDamage" :key="n">
            <dt>{{ $t('campaign.scenario', { n }) }} · {{ $t('campaign.hopeDamage') }}</dt>
            <dd>{{ damage }}</dd>
          </template>
        </dl>
        <dl v-else-if="mutant" class="scenario-log">
          <template v-for="(code, i) in mutantSchemes" :key="code">
            <dt>{{ $t('campaign.scenario', { n: i + 1 }) }} · {{ name(code) }}</dt>
            <dd>{{ log.mutantGenesis.defeatedSchemes.includes(code) ? '✓' : '—' }}</dd>
          </template>
          <dt>{{ $t('campaign.futurePastActive') }}</dt>
          <dd>{{ names(log.mutantGenesis.futurePastActive) }}</dd>
          <dt>{{ $t('campaign.futurePastRemoved') }}</dt>
          <dd>{{ names(log.mutantGenesis.futurePastRemoved) }}</dd>
          <dt>{{ $t('campaign.roleUpgradesRemoved') }}</dt>
          <dd>{{ names(log.mutantGenesis.roleUpgradesRemoved) }}</dd>
          <dt>{{ $t('campaign.jubilee') }}</dt>
          <dd>{{ log.mutantGenesis.jubileeInPlay ? '✓' : '—' }}</dd>
          <dt>{{ $t('campaign.rescuedAllies') }}</dt>
          <dd>{{ names(log.mutantGenesis.captiveAllies) }}</dd>
          <dt>{{ $t('campaign.alliesRemoved') }}</dt>
          <dd>{{ names(log.alliesRemoved) }}</dd>
        </dl>
        <dl v-else-if="sinister" class="scenario-log">
          <dt>{{ $t('campaign.reputation') }}</dt>
          <dd>{{ log.sinister.reputation }}</dd>
          <dt>{{ $t('campaign.communityService') }}</dt>
          <dd>{{ names(log.sinister.communityService) }}</dd>
          <dt>{{ $t('campaign.osbornTech') }}</dt>
          <dd>{{ names(log.sinister.osbornTech) }}</dd>
          <dt>{{ $t('campaign.wakingNightmare') }}</dt>
          <dd>{{ log.sinister.wakingNightmare ?? $t('campaign.none') }}</dd>
          <dt>{{ $t('campaign.lastOnesStanding') }}</dt>
          <dd>{{ names(log.sinister.lastOnesStanding) }}</dd>
          <dt v-if="log.sinister.finalReputation !== null">{{ $t('campaign.finalReputation') }}</dt>
          <dd v-if="log.sinister.finalReputation !== null">{{ log.sinister.finalReputation }}</dd>
        </dl>
        <dl v-else-if="titan" class="scenario-log">
          <template v-for="[n, marks] in titanMarks" :key="n">
            <template v-for="m in marks" :key="m">
              <dt>{{ $t('campaign.scenario', { n }) }} · {{ $t(`campaign.titanMarks.${m}`) }}</dt>
              <dd>{{ marked(m) ? '✓' : '—' }}</dd>
            </template>
          </template>
        </dl>
        <dl v-else class="scenario-log">
          <dt>{{ $t('campaign.scenario', { n: 1 }) }} · {{ $t('campaign.experimentalWeapons') }}</dt>
          <dd>{{ names(log.experimentalWeapons) }}</dd>
          <dt>{{ $t('campaign.scenario', { n: 2 }) }} · {{ $t('campaign.delayCounters') }}</dt>
          <dd>{{ log.delayCounters ?? $t('campaign.none') }}</dd>
          <dt>{{ $t('campaign.scenario', { n: 4 }) }} · {{ $t('campaign.engagedWithMinions') }}</dt>
          <dd>{{ names(log.engagedWithMinions) }}</dd>
          <dt>{{ $t('campaign.scenario', { n: 4 }) }} · {{ $t('campaign.alliesRemoved') }}</dt>
          <dd>{{ names(log.alliesRemoved) }}</dd>
        </dl>
      </section>
    </div>
    <div v-else-if="error" class="page-content"><p class="error">{{ tr(error) }}</p></div>

    <Teleport to="body">
      <StoryReading v-if="guideOpen && view" :sections="campaignGuideSections(view.campaign.kind, view.campaign.mode)" :can-continue="false" reference :source-url="campaignGuides[view.campaign.kind].source" @close="guideOpen = false" />
    </Teleport>

    <dialog ref="dialog" class="picker-dialog" @close="changing = null" @click="$event.target === dialog && dialog?.close()">
      <div v-if="changing !== null && seatOf(changing)" class="picker-body">
        <header>
          <h2>{{ $t('campaign.changeDeckTitle', { hero: name(seatOf(changing)!.hero) }) }}</h2>
          <button type="button" class="btn ghost" @click="dialog?.close()">{{ $t('common.close') }}</button>
        </header>
        <DeckPicker v-model="picking" v-model:loading="pickingLoad" :decks="decks" :starters="starters" :loaded="loaded?.face ?? null" @load="(face, list) => (loaded = { face, list })" @saved="async () => (decks = await fetchDecks().catch(() => decks))" />
        <button v-if="!pickingLoad" type="button" class="btn primary big" :disabled="working" @click="useDeck">{{ $t('campaign.changeDeck') }}</button>
      </div>
    </dialog>
  </div>
</template>

<style scoped>
.campaign { display: grid; gap: 18px; }
.head { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 12px; }
.head > div { flex: 1 1 16rem; min-width: 0; margin-right: auto; }
.head .page-title { margin: 0; }
.eyebrow { margin: 0 0 4px; font-size: 0.75rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--amber); }
h2 { margin: 0 0 12px; font-size: 1.2rem; text-transform: uppercase; color: var(--title); }

.track { display: grid; grid-template-columns: repeat(auto-fill, minmax(min(220px, 100%), 1fr)); gap: 14px; margin: 0; padding: 0; list-style: none; }
.stop { min-width: 0; }

.next { display: flex; flex-wrap: wrap; align-items: center; gap: 16px; }
.progress { margin: 0 auto 0 0; color: var(--text-muted); }
.result { margin: 0; font-weight: 700; }
.result.good { color: var(--amber); }
.result.bad { color: var(--danger); }

.players { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: 12px; }
.player { display: grid; gap: 10px; padding: 12px; border-radius: 12px; background: var(--ink-3); border: 1px solid var(--line); }
.player header { display: flex; align-items: center; gap: 10px; }
.player header div { display: grid; }
.player small { font-size: 0.7rem; letter-spacing: 0.1em; text-transform: uppercase; color: var(--text-faint); }
.player .portrait { width: 44px; height: 44px; flex: none; border-radius: 50%; background-size: cover; background-position: center top; }
dl { display: grid; grid-template-columns: auto minmax(0, 1fr); gap: 4px 12px; margin: 0; font-size: 0.88rem; }
dt { color: var(--text-muted); }
dd { margin: 0; }
.market { display: grid; gap: 4px; margin: 0; padding: 0; list-style: none; }
.market .hint { font-size: 0.8rem; color: var(--text-muted); }
.pending { margin: 0; font-size: 0.8rem; color: var(--amber); }
.short { margin: 0; font-size: 0.8rem; color: var(--danger); }
.picker-dialog { width: min(62rem, 100vw - 2rem); max-height: min(48rem, 100dvh - 2rem); padding: 0; border: 1px solid var(--line-strong); border-radius: 16px; background: var(--ink-2); color: var(--text); }
.picker-dialog::backdrop { background: rgb(0 0 0 / 0.6); }
.picker-body { display: grid; gap: 14px; padding: 18px; }
.picker-body header { display: flex; align-items: center; justify-content: space-between; }
.picker-body h2 { margin: 0; font-size: 1.4rem; text-transform: uppercase; }
.scenario-log { grid-template-columns: minmax(0, 22rem) minmax(0, 1fr); }
.error { margin: 0; padding: 10px 14px; border-radius: 10px; border: 1px solid var(--danger); color: var(--danger); }

@media (max-width: 760px) {
  .track { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  .scenario-log { grid-template-columns: minmax(0, 1fr); }
}

.fne-progress { width: 100%; border-collapse: collapse; margin-top: 1rem; }
.fne-progress th, .fne-progress td { text-align: left; padding: .65rem .4rem; border-bottom: 1px solid var(--border, #ddd); }
</style>
