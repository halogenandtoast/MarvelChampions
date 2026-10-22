<script lang="ts" setup>
import { computed, onMounted, reactive, ref, useTemplateRef, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { Check, Swords, UserPlus } from 'lucide-vue-next'
import { createCampaign, createGame, fetchScenarios, fetchStarterDecks, type DeckChoice } from '../api'
import { currentUser, updateSettings } from '../auth'
import { fetchDecks } from '../browser/lib/api'
import type { DeckList, DeckSummary } from '../browser/types/deck'
import { useCards } from '../cards'
import DeckPicker from '../components/DeckPicker.vue'
import DeckTile from '../components/DeckTile.vue'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import { deckFaces, toDeckChoice, type DeckFace, type DeckKey } from '../deckChoice'
import { useServerText } from '../i18n'
import type { CampaignMode, ScenarioOption, StarterDeck } from '../server'

const tr = useServerText()
const { t } = useI18n()
const router = useRouter()
const cards = useCards()

const scenarios = ref<ScenarioOption[]>([])
const modularSets = ref<string[]>([])
const starters = ref<StarterDeck[]>([])
const decks = ref<DeckSummary[]>([])
const error = ref<string | null>(null)
const creating = ref(false)

const scenario = ref<ScenarioOption | null>(null)
const modulars = ref<string[]>([])
const difficulty = ref<'Normal' | 'Expert'>('Normal')
const gameName = ref('')

// Each seat brings a deck, or, playing with friends, is left open for one to take.
interface Seat {
  deck: DeckKey
  open: boolean
  loaded: { face: DeckFace; list: DeckList } | null
}
const seatCount = ref(1)
const withFriends = ref(false)
const defaultStarters = ['spider-man', 'captain-marvel', 'she-hulk', 'iron-man']
const seats = reactive<Seat[]>(defaultStarters.map((key) => ({ deck: `starter:${key}`, open: false, loaded: null })))
// a deck's page sends you here to play it: `?deck=<id>` sits in the first seat
const asked = useRoute().query.deck
if (typeof asked === 'string' && /^\d+$/.test(asked)) seats[0].deck = `deck:${asked}`
const used = computed(() => seats.slice(0, seatCount.value))
const taking = computed(() => used.value.filter((s) => !withFriends.value || !s.open))

onMounted(async () => {
  try {
    const [list, startersList] = await Promise.all([fetchScenarios(), fetchStarterDecks()])
    ;[scenarios.value, modularSets.value, starters.value] = [list.scenarios, list.modulars, startersList]
    if (!scenario.value && sortedScenarios.value[0]) choose(sortedScenarios.value[0])
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
  }
  decks.value = await fetchDecks().catch(() => [])
})

const choose = (option: ScenarioOption) => {
  scenario.value = option
  modulars.value = [...option.modulars]
}
const toggleModular = (set: string) => {
  modulars.value = modulars.value.includes(set) ? modulars.value.filter((s) => s !== set) : [...modulars.value, set]
}
const villainArt = (code: string) => cards.sources(code).map((s) => `url('${s}')`).join(', ')

// Releases, scenarios and modular sets are named by the card data, so a new box needs only
// its cards: a scenario by its villain's card, a modular set by any card in it.
interface Release { code: string; name: string; order: number }
const NO_RELEASE: Release = { code: '', name: '', order: Infinity }
const releaseOf = (code: string | null | undefined, card = cards.card(code)): Release =>
  card ? { code: card.packCode, name: card.packName ?? card.packCode, order: card.packOrder ?? Infinity } : NO_RELEASE
// the engine's BombScare is MarvelCDB's bomb_scare, save for the sets MarvelCDB abbreviates
const SET_CODES: Record<string, string> = { ExperimentalWeapons: 'exper_weapon', WeaponMaster: 'weap_master', MasterOfTime: 'mot', ExpertKang: 'exp_kang', ExpertCampaign: 'expcamp' }
const setCode = (set: string) => SET_CODES[set] ?? set.replace(/(?<!^)([A-Z])/g, '_$1').toLowerCase()
const setCards = computed(() => {
  const bySet = new Map<string, (typeof cards.store.all)[number]>()
  for (const c of cards.store.all) if (c.setCode && !bySet.has(c.setCode)) bySet.set(c.setCode, c)
  return bySet
})
const modularName = (set: string) => setCards.value.get(setCode(set))?.setName ?? set
const modularRelease = (set: string) => releaseOf(null, setCards.value.get(setCode(set)))
// a villain may have several scenarios, as the Green Goblin has: each is named for its set
const scenarioName = (option: ScenarioOption) => cards.card(option.code)?.setName ?? option.name
const villainName = (option: ScenarioOption) => cards.name(option.code, option.name)

const byRelease = <T,>(items: T[], release: (item: T) => Release, name: (item: T) => string) =>
  [...items].sort((a, b) => release(a).order - release(b).order || name(a).localeCompare(name(b)))
const releasesOf = <T,>(items: T[], release: (item: T) => Release) =>
  [...new Map(items.map((i) => [release(i).code, release(i)])).values()].filter((r) => r.code).sort((a, b) => a.order - b.order)

// in the order the box has them, which their codes follow
const sortedScenarios = computed(() => byRelease(scenarios.value, (o) => releaseOf(o.code), (o) => o.code))
const scenarioReleases = computed(() => releasesOf(scenarios.value, (o) => releaseOf(o.code)))
const villainFilter = ref('')
const shownScenarios = computed(() => sortedScenarios.value.filter((o) => !villainFilter.value || releaseOf(o.code).code === villainFilter.value))

const modularReleases = computed(() => releasesOf(modularSets.value, modularRelease))
const modularFilter = ref('')
const modularSearch = ref('')
// the sets chosen stay in view, whatever the filter, so they can be taken out again
const shownModulars = computed(() => {
  const search = modularSearch.value.trim().toLowerCase()
  return byRelease(modularSets.value, modularRelease, modularName).filter(
    (set) =>
      modulars.value.includes(set) ||
      ((!modularFilter.value || modularRelease(set).code === modularFilter.value) && (!search || modularName(set).toLowerCase().includes(search))),
  )
})

// what each seat plays, to show it and to keep heroes unique
const faces = computed(() => deckFaces(starters.value, decks.value))
const faceOf = (seat: Seat): DeckFace | null => (seat.deck === 'loaded' ? (seat.loaded?.face ?? null) : (faces.value.find((f) => f.key === seat.deck) ?? null))
const takenBy = (index: number) => taking.value.filter((s) => s !== used.value[index]).flatMap((s) => (faceOf(s) ? [faceOf(s)!.hero] : []))
const choice = (seat: Seat): DeckChoice => (seat.deck === 'loaded' && seat.loaded ? { list: seat.loaded.list } : toDeckChoice(seat.deck))

// a seat's deck is chosen in a dialog over the page
const dialog = useTemplateRef<HTMLDialogElement>('dialog')
const picking = ref<number | null>(null)
const pickingLoad = ref(false)
const pick = (index: number) => {
  picking.value = index
  dialog.value?.showModal()
}
const picked = computed({
  get: () => (picking.value === null ? '' : seats[picking.value].deck),
  set: (key: DeckKey) => {
    if (picking.value === null) return
    seats[picking.value].deck = key
    if (!pickingLoad.value) dialog.value?.close()
  },
})
const onLoad = (face: DeckFace, list: DeckList) => {
  if (picking.value !== null) seats[picking.value].loaded = { face, list }
}
watch(pickingLoad, (loading, was) => {
  if (was && !loading && picking.value !== null && seats[picking.value].deck === 'loaded') dialog.value?.close()
})

// As in Arkham: decided when the game is made, starting from the player's default.
const allowSpectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const differsFromDefault = computed(() => allowSpectators.value !== (currentUser.value?.allowSpectatorsByDefault === true))
const savedDefault = ref(false)
watch(allowSpectators, () => (savedDefault.value = false))
const makeDefault = async () => {
  await updateSettings({ allowSpectatorsByDefault: allowSpectators.value })
  savedDefault.value = true
}

const defaultName = computed(() => (scenario.value ? t('home.versus', { villain: villainName(scenario.value) }) : ''))
const canCreate = computed(() => !!scenario.value && !creating.value && (withFriends.value || taking.value.length > 0))

const create = async () => {
  if (!canCreate.value || !scenario.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createGame({
      name: gameName.value.trim() || defaultName.value,
      scenario: { scenarioCode: scenario.value.code, modulars: modulars.value, difficulty: difficulty.value },
      decks: taking.value.map(choice),
      seats: seatCount.value,
      allowSpectators: allowSpectators.value,
    })
    await router.push(`/games/${id}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
    creating.value = false
  }
}

// A campaign instead of one scenario: its heroes are chosen now and kept to the end, and
// its overview deals each scenario in turn.
type Kind = 'scenario' | 'campaign'
const kind = ref<Kind>('scenario')
const kindOptions = computed(() => [
  { value: 'scenario' as Kind, label: t('newGame.singleScenario') },
  { value: 'campaign' as Kind, label: t('newGame.campaign') },
])
const campaignMode = ref<CampaignMode>('StandardCampaign')
const campaignModeOptions = computed(() => [
  { value: 'StandardCampaign' as CampaignMode, label: t('campaign.StandardCampaign') },
  { value: 'ExpertCampaign' as CampaignMode, label: t('campaign.ExpertCampaign') },
])
// the villains of The Rise of Red Skull, in order
const RED_SKULL_CAMPAIGN = ['04058', '04076', '04093', '04109', '04125']
watch(kind, (k) => {
  if (k === 'campaign') withFriends.value = false
})
const canCreateCampaign = computed(() => !creating.value && used.value.length > 0)
const createCampaignNow = async () => {
  if (!canCreateCampaign.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createCampaign({
      name: gameName.value.trim() || t('campaign.riseOfRedSkull'),
      mode: campaignMode.value,
      difficulty: difficulty.value,
      decks: used.value.map(choice),
    })
    await router.push(`/campaigns/${id}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
    creating.value = false
  }
}
const submit = () => (kind.value === 'campaign' ? createCampaignNow() : create())

const difficultyOptions = computed(() => [
  { value: 'Normal' as const, label: t('difficulty.Normal') },
  { value: 'Expert' as const, label: t('difficulty.Expert') },
])
const playerOptions = computed(() => [1, 2, 3, 4].map((n) => ({ value: String(n), label: String(n) })))
</script>

<template>
  <div class="page-container">
    <form class="page-content new-game" @submit.prevent="submit">
      <header class="head">
        <h1 class="page-title">{{ $t('newGame.title') }}</h1>
        <RouterLink to="/" class="btn ghost">{{ $t('common.cancel') }}</RouterLink>
      </header>

      <SegmentedToggle v-model="kind" class="kind" :options="kindOptions" :label="$t('newGame.kind')" />

      <section v-if="kind === 'campaign'" class="step">
        <h2 class="step-title"><span class="n">1</span>{{ $t('newGame.chooseCampaign') }}</h2>
        <div class="villains" role="radiogroup">
          <button type="button" role="radio" class="villain campaign-tile selected" aria-checked="true">
            <span class="campaign-art">
              <span v-for="code in RED_SKULL_CAMPAIGN" :key="code" class="art portrait" :style="{ backgroundImage: villainArt(code) }" />
            </span>
            <span class="villain-name">{{ $t('campaign.riseOfRedSkull') }}</span>
            <span class="villain-sets">{{ RED_SKULL_CAMPAIGN.map((c) => cards.name(c, c)).join(' · ') }}</span>
            <Check class="tick" aria-hidden="true" />
          </button>
        </div>
      </section>

      <section v-if="kind === 'scenario'" class="step">
        <div class="step-head">
          <h2 class="step-title"><span class="n">1</span>{{ $t('newGame.chooseVillain') }}</h2>
          <select v-model="villainFilter" class="input filter" :aria-label="$t('newGame.release')">
            <option value="">{{ $t('newGame.allReleases') }}</option>
            <option v-for="r in scenarioReleases" :key="r.code" :value="r.code">{{ r.name }}</option>
          </select>
        </div>
        <div class="villains" role="radiogroup">
          <button
            v-for="option in shownScenarios"
            :key="option.code"
            type="button"
            role="radio"
            class="villain"
            :class="{ selected: scenario?.code === option.code }"
            :aria-checked="scenario?.code === option.code"
            @click="choose(option)"
          >
            <span class="art portrait" :style="{ backgroundImage: villainArt(option.code) }" />
            <span class="villain-name">{{ villainName(option) }}</span>
            <span v-if="scenarioName(option) !== villainName(option)" class="scenario-name">{{ scenarioName(option) }}</span>
            <span class="villain-sets">{{ releaseOf(option.code).name }} · {{ option.modulars.map(modularName).join(', ') }}</span>
            <Check v-if="scenario?.code === option.code" class="tick" aria-hidden="true" />
          </button>
        </div>
      </section>

      <section v-if="kind === 'campaign'" class="step two">
        <div class="col">
          <h2 class="step-title"><span class="n">2</span>{{ $t('newGame.campaignMode') }}</h2>
          <SegmentedToggle v-model="campaignMode" :options="campaignModeOptions" :label="$t('newGame.campaignMode')" />
          <p class="hint">{{ $t('newGame.campaignModeHint') }}</p>

          <h3 class="field-title">{{ $t('newGame.difficulty') }}</h3>
          <SegmentedToggle v-model="difficulty" :options="difficultyOptions" :label="$t('newGame.difficulty')" />
          <p class="hint">{{ $t('newGame.scenarioDifficultyHint') }}</p>

          <label class="field">
            <span class="field-title">{{ $t('newGame.campaignName') }}</span>
            <input v-model="gameName" class="input" type="text" :placeholder="$t('campaign.riseOfRedSkull')" />
          </label>
        </div>

        <div class="col">
          <h2 class="step-title"><span class="n">3</span>{{ $t('newGame.players') }}</h2>
          <SegmentedToggle :model-value="String(seatCount)" :options="playerOptions" :label="$t('newGame.players')" @update:model-value="seatCount = Number($event)" />
          <p class="hint">{{ $t('newGame.campaignHeroes') }}</p>
          <ol class="seats">
            <li v-for="(seat, i) in used" :key="i" class="seat">
              <span class="seat-label">{{ $t('newGame.seat', { n: i + 1 }) }}</span>
              <button type="button" class="seat-deck" @click="pick(i)">
                <DeckTile v-if="faceOf(seat)" :face="faceOf(seat)!" />
                <span v-else class="choose">{{ $t('newGame.chooseDeck') }}</span>
              </button>
              <div class="seat-actions">
                <button type="button" class="btn ghost" @click="pick(i)">{{ $t('newGame.changeDeck') }}</button>
              </div>
            </li>
          </ol>
        </div>
      </section>

      <section v-if="kind === 'scenario' && scenario" class="step two">
        <div class="col">
          <h2 class="step-title"><span class="n">2</span>{{ $t('newGame.modulars') }}</h2>
          <div class="filters">
            <input v-model="modularSearch" class="input" type="search" :placeholder="$t('newGame.searchSets')" :aria-label="$t('newGame.searchSets')" />
            <select v-model="modularFilter" class="input" :aria-label="$t('newGame.release')">
              <option value="">{{ $t('newGame.allReleases') }}</option>
              <option v-for="r in modularReleases" :key="r.code" :value="r.code">{{ r.name }}</option>
            </select>
          </div>
          <div class="chips">
            <button
              v-for="set in shownModulars"
              :key="set"
              type="button"
              class="set-chip"
              :class="{ on: modulars.includes(set) }"
              :aria-pressed="modulars.includes(set)"
              @click="toggleModular(set)"
            >
              {{ modularName(set) }}
              <small v-if="scenario.modulars.includes(set)">{{ $t('newGame.recommended') }}</small>
            </button>
          </div>
          <p class="hint">{{ $t('newGame.modularsHint') }}</p>

          <h3 class="field-title">{{ $t('newGame.difficulty') }}</h3>
          <SegmentedToggle v-model="difficulty" :options="difficultyOptions" :label="$t('newGame.difficulty')" />
          <p class="hint">{{ $t('newGame.difficultyHint') }}</p>

          <label class="field">
            <span class="field-title">{{ $t('newGame.gameName') }}</span>
            <input v-model="gameName" class="input" type="text" :placeholder="defaultName" />
          </label>

          <div class="toggle-row">
            <label class="switch">
              <input v-model="allowSpectators" type="checkbox" role="switch" />
              <span class="track" aria-hidden="true" />
              {{ $t('newGame.allowSpectators') }}
            </label>
            <button v-if="differsFromDefault" type="button" class="link" @click="makeDefault">{{ $t('newGame.makeDefault') }}</button>
            <span v-else-if="savedDefault" class="hint">{{ $t('newGame.defaultSaved') }}</span>
          </div>
          <p class="hint">{{ $t('newGame.allowSpectatorsHint') }}</p>
        </div>

        <div class="col">
          <h2 class="step-title"><span class="n">3</span>{{ $t('newGame.players') }}</h2>
          <SegmentedToggle :model-value="String(seatCount)" :options="playerOptions" :label="$t('newGame.players')" @update:model-value="seatCount = Number($event)" />
          <div class="toggle-row">
            <label class="switch">
              <input v-model="withFriends" type="checkbox" role="switch" />
              <span class="track" aria-hidden="true" />
              {{ $t('newGame.playWithFriends') }}
            </label>
          </div>
          <p class="hint">{{ $t('newGame.playWithFriendsHint') }}</p>

          <ol class="seats">
            <li v-for="(seat, i) in used" :key="i" class="seat" :class="{ open: withFriends && seat.open }">
              <span class="seat-label">{{ $t('newGame.seat', { n: i + 1 }) }}</span>
              <template v-if="withFriends && seat.open">
                <div class="open-seat"><UserPlus aria-hidden="true" /><span>{{ $t('newGame.openSeat') }}<small>{{ $t('newGame.openSeatHint') }}</small></span></div>
              </template>
              <button v-else type="button" class="seat-deck" @click="pick(i)">
                <DeckTile v-if="faceOf(seat)" :face="faceOf(seat)!" />
                <span v-else class="choose">{{ $t('newGame.chooseDeck') }}</span>
              </button>
              <div class="seat-actions">
                <button v-if="!(withFriends && seat.open)" type="button" class="btn ghost" @click="pick(i)">{{ $t('newGame.changeDeck') }}</button>
                <button v-if="withFriends" type="button" class="btn ghost" @click="seat.open = !seat.open">
                  {{ seat.open ? $t('newGame.takeSeat') : $t('newGame.leaveOpen') }}
                </button>
              </div>
            </li>
          </ol>
        </div>
      </section>

      <p v-if="error" class="error">{{ tr(error) }}</p>

      <div v-if="kind === 'campaign'" class="create">
        <p class="summary">
          {{ $t('newGame.campaignSummary', { campaign: $t('campaign.riseOfRedSkull'), mode: $t(`campaign.${campaignMode}`), players: $t('newGame.playerCount', seatCount) }) }}
        </p>
        <button class="btn primary big" type="submit" :disabled="!canCreateCampaign">
          <Swords aria-hidden="true" />{{ creating ? $t('newGame.creating') : $t('common.create') }}
        </button>
      </div>
      <div v-else-if="scenario" class="create">
        <p class="summary">
          {{ $t('newGame.summary', { villain: scenarioName(scenario), difficulty: $t(`difficulty.${difficulty}`), players: $t('newGame.playerCount', seatCount) }) }}
        </p>
        <button class="btn primary big" type="submit" :disabled="!canCreate">
          <Swords aria-hidden="true" />{{ creating ? $t('newGame.creating') : $t('common.create') }}
        </button>
      </div>
    </form>

    <dialog ref="dialog" class="picker-dialog" @close="picking = null" @click="$event.target === dialog && dialog?.close()">
      <div v-if="picking !== null" class="picker-body">
        <header>
          <h2>{{ $t('newGame.seat', { n: picking + 1 }) }} · {{ $t('newGame.chooseDeck') }}</h2>
          <button type="button" class="btn ghost" @click="dialog?.close()">{{ $t('common.close') }}</button>
        </header>
        <DeckPicker
          v-model="picked"
          v-model:loading="pickingLoad"
          :decks="decks"
          :starters="starters"
          :loaded="seats[picking].loaded?.face ?? null"
          :taken="takenBy(picking)"
          @load="onLoad"
          @saved="async () => (decks = await fetchDecks().catch(() => decks))"
        />
      </div>
    </dialog>
  </div>
</template>

<style scoped>
.new-game { display: grid; gap: 22px; }
.kind { width: 20rem; max-width: 100%; }
.campaign-tile { grid-column: span 2; cursor: default; }
.campaign-art { display: grid; grid-template-columns: repeat(5, 1fr); width: 100%; }
.campaign-art .art { aspect-ratio: 3 / 4; background-size: cover; background-position: center top; }
.head { display: flex; align-items: center; gap: 12px; }
.head .page-title { margin: 0 auto 0 0; }
.step { display: grid; gap: 14px; padding: 20px; border-radius: 16px; background: var(--ink-2); border: 1px solid var(--line); }
.step.two { grid-template-columns: minmax(0, 1fr) minmax(0, 1.25fr); gap: 28px; }
.col { display: flex; flex-direction: column; gap: 10px; min-width: 0; }
.step-title { display: flex; align-items: center; gap: 10px; margin: 0; font-size: 1.3rem; text-transform: uppercase; color: var(--title); }
.n { display: grid; place-items: center; width: 26px; height: 26px; border-radius: 7px; background: var(--crimson); color: #fff; font-size: 0.95rem; }
.field-title { margin: 8px 0 0; font-family: var(--font-ui); font-size: 0.72rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--text-muted); }
.field { display: grid; gap: 6px; }
.hint { margin: 0; font-size: 0.78rem; color: var(--text-faint); }
.step-head { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 10px; }
.filter { width: auto; max-width: 100%; }
.filters { display: flex; flex-wrap: wrap; gap: 8px; }
.filters .input { flex: 1 1 12rem; width: auto; }

.villains { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 14px; }
.villain {
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  padding: 0 0 12px;
  border: 2px solid var(--line);
  border-radius: 14px;
  background: var(--ink-3);
  color: var(--text);
  font: inherit;
  text-align: left;
  overflow: hidden;
  cursor: pointer;
  transition: border-color 0.15s, transform 0.15s;
}
.villain:hover { transform: translateY(-2px); border-color: var(--line-strong); }
.villain.selected { border-color: var(--crimson); box-shadow: 0 0 0 4px rgb(184 50 58 / 0.2), 0 12px 30px rgb(184 50 58 / 0.15); }
.art { position: relative; width: 100%; aspect-ratio: 16 / 10; }
.art::after { content: ''; position: absolute; inset: 0; background: linear-gradient(transparent 50%, var(--ink-3)); }
.villain-name { margin: -18px 14px 0; position: relative; font-family: var(--font-display); font-size: 1.9rem; font-weight: 800; line-height: 1; text-transform: uppercase; }
.scenario-name { margin: 2px 14px 0; position: relative; font-size: 0.85rem; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; color: var(--amber); }
.villain-sets { margin: 4px 14px 0; font-size: 0.75rem; color: var(--text-faint); }
.tick { position: absolute; top: 10px; right: 10px; width: 26px; height: 26px; padding: 4px; border-radius: 50%; background: var(--crimson); color: #fff; }

.chips { display: flex; flex-wrap: wrap; gap: 8px; }
.set-chip {
  display: inline-flex;
  align-items: baseline;
  gap: 6px;
  padding: 7px 12px;
  border: 1px solid var(--line-strong);
  border-radius: 999px;
  background: var(--ink-3);
  color: var(--text-muted);
  font: inherit;
  font-size: 0.85rem;
  font-weight: 600;
  cursor: pointer;
}
.set-chip small { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-faint); }
.set-chip.on { background: rgb(184 50 58 / 0.18); border-color: var(--crimson); color: var(--text); }
.set-chip.on small { color: #ffb3b7; }

.toggle-row { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 16px; margin-top: 6px; }
.link { background: none; border: 0; padding: 0; color: var(--steel-bright); font: inherit; font-size: 0.85rem; text-decoration: underline; cursor: pointer; }

.seats { display: grid; gap: 10px; margin: 6px 0 0; padding: 0; list-style: none; }
.seat { display: grid; grid-template-columns: 64px minmax(0, 1fr) auto; align-items: center; gap: 12px; padding: 10px; border-radius: 12px; background: var(--ink-3); border: 1px solid var(--line); }
.seat-label { font-family: var(--font-display); font-size: 1rem; font-weight: 700; text-transform: uppercase; color: var(--text-muted); }
.seat-deck { padding: 0; border: 0; background: none; font: inherit; cursor: pointer; text-align: left; }
.seat-deck :deep(.deck-tile) { flex-direction: row; align-items: center; }
.seat-deck :deep(.art) { flex: 0 0 96px; aspect-ratio: 4 / 3; }
.choose { display: block; padding: 18px; border: 1px dashed var(--line-strong); border-radius: 10px; color: var(--text-muted); }
.open-seat { display: flex; align-items: center; gap: 10px; padding: 12px; border: 1px dashed var(--line-strong); border-radius: 10px; color: var(--text-muted); }
.open-seat svg { width: 22px; height: 22px; flex: none; }
.open-seat span { display: grid; font-weight: 600; }
.open-seat small { font-weight: 400; font-size: 0.75rem; color: var(--text-faint); }
.seat-actions { display: flex; flex-wrap: wrap; justify-content: flex-end; gap: 4px; }

.error { margin: 0; padding: 10px 14px; border-radius: 10px; border: 1px solid var(--danger); color: var(--danger); }
.create { display: flex; align-items: center; justify-content: flex-end; gap: 18px; }
.summary { margin: 0; color: var(--text-muted); }

.picker-dialog { width: min(62rem, 100vw - 2rem); max-height: min(48rem, 100dvh - 2rem); padding: 0; border: 1px solid var(--line-strong); border-radius: 16px; background: var(--ink-2); color: var(--text); box-shadow: 0 20px 60px var(--shadow-strong); }
.picker-dialog::backdrop { background: rgb(0 0 0 / 0.6); backdrop-filter: blur(3px); }
.picker-body { display: grid; gap: 14px; padding: 18px; }
.picker-body header { display: flex; align-items: center; justify-content: space-between; }
.picker-body h2 { margin: 0; font-size: 1.4rem; text-transform: uppercase; }

@media (max-width: 860px) {
  .step.two { grid-template-columns: minmax(0, 1fr); }
  .seat { grid-template-columns: minmax(0, 1fr); }
  .seat-actions { justify-content: flex-start; }
}
</style>
