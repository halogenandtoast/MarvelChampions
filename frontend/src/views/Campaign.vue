<script lang="ts" setup>
import { computed, onMounted, ref, useTemplateRef } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { Check, Play, Trash2 } from 'lucide-vue-next'
import { changeCampaignDeck, continueCampaign, deleteCampaign, fetchCampaign, fetchStarterDecks, joinCampaign, leaveCampaign, startCampaign, type DeckChoice } from '../api'
import { currentUser } from '../auth'
import { fetchDecks } from '../browser/lib/api'
import type { DeckList, DeckSummary } from '../browser/types/deck'
import { useCards } from '../cards'
import DeckPicker from '../components/DeckPicker.vue'
import Lobby from '../components/Lobby.vue'
import { toDeckChoice, type DeckFace, type DeckKey } from '../deckChoice'
import { useServerText } from '../i18n'
import type { CampaignView, StarterDeck } from '../server'

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
// said once: the campaign's own title only when its players named it something else
const details = computed(() => {
  const v = view.value
  if (!v) return []
  const title = t('campaign.riseOfRedSkull')
  return [...(v.name === title ? [] : [title]), t(`campaign.${v.campaign.mode}`), t(`difficulty.${v.difficulty}`), t('newGame.playerCount', v.table.seats)]
})
const name = (code: string | null | undefined) => (code ? cards.name(code, code) : t('campaign.none'))
const names = (codes: string[]) => (codes.length ? codes.map((c) => name(c)).join(', ') : t('campaign.none'))
const art = (code: string) => cards.sources(code).map((s) => `url('${s}')`).join(', ')
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
        <button v-if="view.table.owner === currentUser?.id" type="button" class="btn ghost" :title="$t('campaign.delete')" @click="remove"><Trash2 aria-hidden="true" />{{ $t('campaign.delete') }}</button>
      </header>

      <ol class="track">
        <li v-for="(code, i) in view.campaign.scenarios" :key="code" class="stop" :class="stage(i)">
          <span class="art" :style="{ backgroundImage: art(code) }" />
          <span class="label">{{ $t('campaign.scenario', { n: i + 1 }) }}</span>
          <span class="villain">{{ name(code) }}</span>
          <Check v-if="stage(i) === 'won'" class="tick" aria-hidden="true" />
        </li>
      </ol>

      <Lobby v-if="view.waitingForPlayers" :table="view.table" @join="join" @leave="leave" @start="begin" />

      <div v-else class="next box">
        <p class="progress">{{ $t('campaign.progress', { done: view.campaign.completed, total: view.campaign.scenarios.length }) }}</p>
        <p v-if="view.won" class="result good">{{ $t('campaign.won') }}</p>
        <p v-else-if="view.campaign.lost" class="result bad">{{ $t('campaign.lost') }}</p>
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
            <dl>
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
            <button v-if="yours(i) && !view.currentGame && !view.waitingForPlayers && view.next" type="button" class="btn ghost" @click="changeDeck(i)">{{ $t('campaign.changeDeck') }}</button>
            <p v-if="p.pending.length" class="pending">{{ $t('campaign.toChoose') }}: {{ p.pending.map((c) => $t(`campaign.${c}`)).join(', ') }}</p>
          </article>
        </div>
      </section>

      <section class="box">
        <h2>{{ $t('campaign.log') }}</h2>
        <dl class="scenario-log">
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
.head { display: flex; align-items: flex-end; gap: 12px; }
.head > div { margin-right: auto; }
.head .page-title { margin: 0; }
.eyebrow { margin: 0 0 4px; font-size: 0.75rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--amber); }
h2 { margin: 0 0 12px; font-size: 1.2rem; text-transform: uppercase; color: var(--title); }

.track { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 10px; margin: 0; padding: 0; list-style: none; }
.stop { position: relative; display: grid; border-radius: 12px; overflow: hidden; border: 2px solid var(--line); background: var(--ink-3); }
.stop .art { aspect-ratio: 4 / 5; background-size: cover; background-position: center top; filter: grayscale(0.7) brightness(0.6); }
.stop.won .art { filter: none; }
.stop.current { border-color: var(--crimson); box-shadow: 0 0 0 4px rgb(184 50 58 / 0.2); }
.stop.current .art { filter: none; }
.stop.lost { border-color: var(--danger); }
.stop .label { padding: 8px 10px 0; font-size: 0.68rem; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: var(--text-faint); }
.stop .villain { padding: 0 10px 10px; font-family: var(--font-display); font-size: 1.15rem; font-weight: 800; text-transform: uppercase; }
.tick { position: absolute; top: 8px; right: 8px; width: 24px; height: 24px; padding: 4px; border-radius: 50%; background: var(--crimson); color: #fff; }

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
.portrait { width: 44px; height: 44px; flex: none; border-radius: 50%; background-size: cover; background-position: center top; }
dl { display: grid; grid-template-columns: auto minmax(0, 1fr); gap: 4px 12px; margin: 0; font-size: 0.88rem; }
dt { color: var(--text-muted); }
dd { margin: 0; }
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
</style>
