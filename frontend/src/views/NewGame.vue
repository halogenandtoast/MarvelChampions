<script lang="ts" setup>
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { ArrowLeft, Check, Users } from 'lucide-vue-next'
import { createCampaign, createGame, fetchScenarios } from '../api'
import { currentUser, updateSettings } from '../auth'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import SetupBar from '../components/SetupBar.vue'
import { campaignArt } from '../campaigns'
import { useServerText } from '../i18n'
import { baseCode, useScenarios, type Release } from '../scenarios'
import type { CampaignKind, CampaignMode, ScenarioOption } from '../server'

const tr = useServerText()
const { t } = useI18n()
const router = useRouter()

const scenarios = ref<ScenarioOption[]>([])
const modularSets = ref<string[]>([])
const error = ref<string | null>(null)
const creating = ref(false)

const scenario = ref<ScenarioOption | null>(null)
const modulars = ref<string[]>([])
const difficulty = ref<'Normal' | 'Expert'>('Normal')
const gameName = ref('')

const seatCount = ref(1)
// a deck's page sends you here to play it: the lobby starts on it
const asked = useRoute().query.deck
const deckQuery = typeof asked === 'string' && /^\d+$/.test(asked) ? { deck: asked } : {}

// Villain or campaign, then its settings; the game is made with every seat open, and its
// lobby is where each seat takes a deck.
const step = ref<1 | 2>(1)

onMounted(async () => {
  try {
    const list = await fetchScenarios()
    ;[scenarios.value, modularSets.value] = [list.scenarios, list.modulars]
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
  }
})

// a villain picked goes on to its setup; picked again, it keeps what was set
const pickVillain = (option: ScenarioOption) => {
  if (!scenario.value || baseCode(scenario.value.code) !== baseCode(option.code)) choose(option)
  step.value = 2
}
const choose = (option: ScenarioOption) => {
  scenario.value = option
  modulars.value = [...option.modulars]
}
const toggleModular = (set: string) => {
  modulars.value = modulars.value.includes(set) ? modulars.value.filter((s) => s !== set) : [...modulars.value, set]
}
const { releaseOf, modularName, modularRelease, portraits, art: villainArt, variant, tileName, scenarioName, villainName, details } = useScenarios(scenarios)

const byRelease = <T,>(items: T[], release: (item: T) => Release, name: (item: T) => string) =>
  [...items].sort((a, b) => release(a).order - release(b).order || name(a).localeCompare(name(b)))
const releasesOf = <T,>(items: T[], release: (item: T) => Release) =>
  [...new Map(items.map((i) => [release(i).code, release(i)])).values()].filter((r) => r.code).sort((a, b) => a.order - b.order)

// in the order the box has them, which their codes follow
const sortedScenarios = computed(() => byRelease(scenarios.value, (o) => releaseOf(o.code), (o) => o.code))
const scenarioReleases = computed(() => releasesOf(scenarios.value, (o) => releaseOf(o.code)))
const villainFilter = ref('')
// a scenario's variants, as Breakout's, share its tile and are chosen among on the next step
const shownScenarios = computed(() =>
  sortedScenarios.value.filter(
    (o, i, all) => (!villainFilter.value || releaseOf(o.code).code === villainFilter.value) && all.findIndex((x) => baseCode(x.code) === baseCode(o.code)) === i,
  ),
)
const versions = computed(() => (scenario.value ? sortedScenarios.value.filter((o) => baseCode(o.code) === baseCode(scenario.value!.code)) : []))
const versionOptions = computed(() =>
  versions.value.map((o) => ({ value: o.code, label: t(`newGame.version.${variant(o.code) ?? 'standard'}`) })),
)

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

// As in Arkham: decided when the game is made, starting from the player's default.
const allowSpectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const differsFromDefault = computed(() => allowSpectators.value !== (currentUser.value?.allowSpectatorsByDefault === true))
const savedDefault = ref(false)
watch(allowSpectators, () => (savedDefault.value = false))
const makeDefault = async () => {
  await updateSettings({ allowSpectatorsByDefault: allowSpectators.value })
  savedDefault.value = true
}

const defaultName = computed(() => (scenario.value ? t('home.versus', { villain: villainName(scenario.value.code) }) : ''))
const canCreate = computed(() => !!scenario.value && !creating.value)

const create = async () => {
  if (!canCreate.value || !scenario.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createGame({
      name: gameName.value.trim() || defaultName.value,
      scenario: { scenarioCode: scenario.value.code, modulars: modulars.value, difficulty: difficulty.value },
      decks: [],
      seats: seatCount.value,
      allowSpectators: allowSpectators.value,
    })
    await router.push({ path: `/games/${id}`, query: deckQuery })
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
// which box's campaign
const campaignKind = ref<CampaignKind>('RiseOfRedSkull')
const campaignKinds: CampaignKind[] = ['RiseOfRedSkull', 'GalaxysMostWanted', 'MadTitansShadow']
const campaignTitle = computed(() => t(`campaign.${campaignKind.value}`))
const campaignModeOptions = computed(() => [
  { value: 'StandardCampaign' as CampaignMode, label: t('campaign.StandardCampaign') },
  { value: 'ExpertCampaign' as CampaignMode, label: t('campaign.ExpertCampaign') },
])
const canCreateCampaign = computed(() => !creating.value)
const createCampaignNow = async () => {
  if (!canCreateCampaign.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createCampaign({
      name: gameName.value.trim() || campaignTitle.value,
      campaign: campaignKind.value,
      mode: campaignMode.value,
      difficulty: difficulty.value,
      decks: [],
      seats: seatCount.value,
    })
    await router.push({ path: `/campaigns/${id}`, query: deckQuery })
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
    <form class="page-content new-game" @submit.prevent="step === 2 && submit()">
      <header class="head">
        <h1 class="page-title">{{ $t('newGame.title') }}</h1>
        <RouterLink to="/" class="btn ghost">{{ $t('common.cancel') }}</RouterLink>
      </header>

      <ol class="steps">
        <li v-for="(label, i) in [kind === 'campaign' ? $t('newGame.chooseCampaign') : $t('newGame.chooseVillain'), $t('newGame.setup'), $t('game.lobby.eyebrow')]" :key="i" :class="{ current: step === i + 1, done: step > i + 1 }">
          <span class="n">{{ i + 1 }}</span>{{ label }}
        </li>
      </ol>

      <SetupBar
        v-if="step === 2 && (kind === 'campaign' || scenario)"
        :portraits="kind === 'campaign' ? [] : portraits(scenario!.code)"
        :campaign="kind === 'campaign' ? campaignKind : undefined"
        :name="kind === 'campaign' ? campaignTitle : scenarioName(scenario!.code)"
        :details="
          (kind === 'campaign' ? [$t(`campaign.${campaignMode}`), $t(`difficulty.${difficulty}`)] : details({ scenarioCode: scenario!.code, modulars, difficulty })).concat(
            $t('newGame.playerCount', seatCount),
          )
        "
      >
        <button type="button" class="btn ghost" @click="step = 1">{{ $t('newGame.change') }}</button>
      </SetupBar>

      <template v-if="step === 1">
        <SegmentedToggle v-model="kind" class="kind" :options="kindOptions" :label="$t('newGame.kind')" />

        <section v-if="kind === 'campaign'" class="step">
          <div class="villains" role="radiogroup">
            <button
              v-for="k in campaignKinds"
              :key="k"
              type="button"
              role="radio"
              class="villain campaign-tile"
              :class="{ selected: campaignKind === k }"
              :aria-checked="campaignKind === k"
              @click="(campaignKind = k), (step = 2)"
            >
              <span class="box-art" :style="{ backgroundImage: `url('${campaignArt[k]}')` }" />
              <span class="villain-name">{{ $t(`campaign.${k}`) }}</span>
              <Check v-if="campaignKind === k" class="tick" aria-hidden="true" />
            </button>
          </div>
        </section>

        <section v-else class="step">
          <div class="step-head">
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
              :class="{ selected: scenario && baseCode(scenario.code) === baseCode(option.code) }"
              :aria-checked="!!scenario && baseCode(scenario.code) === baseCode(option.code)"
              @click="pickVillain(option)"
            >
              <span class="art-strip">
                <span v-for="code in portraits(option.code)" :key="code" class="art portrait" :data-code="code" :style="{ backgroundImage: villainArt(code) }" />
              </span>
              <span class="villain-name">{{ tileName(option.code) }}</span>
              <span v-if="releaseOf(option.code).name !== tileName(option.code)" class="villain-sets">{{ releaseOf(option.code).name }}</span>
              <Check v-if="scenario && baseCode(scenario.code) === baseCode(option.code)" class="tick" aria-hidden="true" />
            </button>
          </div>
        </section>
      </template>

      <section v-else-if="kind === 'campaign'" class="step two">
        <div class="col">
          <h3 class="field-title">{{ $t('newGame.campaignMode') }}</h3>
          <SegmentedToggle v-model="campaignMode" :options="campaignModeOptions" :label="$t('newGame.campaignMode')" />
          <p class="hint">{{ $t('newGame.campaignModeHint') }}</p>

          <h3 class="field-title">{{ $t('newGame.difficulty') }}</h3>
          <SegmentedToggle v-model="difficulty" :options="difficultyOptions" :label="$t('newGame.difficulty')" />
          <p class="hint">{{ $t('newGame.scenarioDifficultyHint') }}</p>
        </div>

        <div class="col">
          <h3 class="field-title">{{ $t('newGame.players') }}</h3>
          <SegmentedToggle :model-value="String(seatCount)" :options="playerOptions" :label="$t('newGame.players')" @update:model-value="seatCount = Number($event)" />
          <p class="hint">{{ $t('newGame.campaignPlayersHint') }}</p>

          <label class="field">
            <span class="field-title">{{ $t('newGame.campaignName') }}</span>
            <input v-model="gameName" class="input" type="text" :placeholder="campaignTitle" />
          </label>
        </div>
      </section>

      <section v-else-if="scenario" class="step two">
        <div class="col">
          <template v-if="versions.length > 1">
            <h3 class="field-title">{{ $t('newGame.version.title') }}</h3>
            <SegmentedToggle :model-value="scenario.code" :options="versionOptions" :label="$t('newGame.version.title')" @update:model-value="choose(versions.find((v) => v.code === $event)!)" />
            <p class="hint">{{ $t('newGame.version.hint') }}</p>
          </template>
          <h3 class="field-title">{{ $t('newGame.modulars') }}</h3>
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
        </div>

        <div class="col">
          <h3 class="field-title">{{ $t('newGame.players') }}</h3>
          <SegmentedToggle :model-value="String(seatCount)" :options="playerOptions" :label="$t('newGame.players')" @update:model-value="seatCount = Number($event)" />
          <p class="hint">{{ $t('newGame.playersHint') }}</p>

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
      </section>

      <p v-if="error" class="error">{{ tr(error) }}</p>

      <div v-if="step === 2" class="create">
        <button type="button" class="btn big back" @click="step = 1"><ArrowLeft aria-hidden="true" />{{ $t('newGame.back') }}</button>
        <button class="btn primary big" type="submit" :disabled="kind === 'campaign' ? !canCreateCampaign : !canCreate">
          <Users aria-hidden="true" />{{ creating ? $t('newGame.creating') : $t('newGame.openLobby') }}
        </button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.new-game { display: grid; gap: 22px; }
.kind { width: 20rem; max-width: 100%; }
.box-art { display: block; width: 100%; aspect-ratio: 775 / 830; background: center / contain no-repeat; }
.campaign-tile .villain-name { margin-top: 12px; font-size: 1.6rem; }
.art-strip { display: grid; grid-auto-flow: column; grid-auto-columns: 1fr; width: 100%; aspect-ratio: 16 / 10; }
.art-strip .art { aspect-ratio: auto; height: 100%; }
.art-strip .art[data-code='16103'] { background-position: 95% 12%; }
.art-strip .art:not(:only-child) { background-size: auto 210%; background-position: 72% 12%; }
.art-strip .art[data-code='07002'] { background-position: calc(72% + 16px) calc(12% - 8px); }
.art-strip .art[data-code='07017'] { background-position: calc(72% + 32px) calc(12% - 8px); }
.art-strip .art[data-code='07032'] { background-position: calc(72% + 48px) calc(12% - 8px); }
.art-strip .art[data-code='07046'] { background-position: 72% calc(12% - 8px); }
.head { display: flex; align-items: center; gap: 12px; }
.head .page-title { margin: 0 auto 0 0; }
.step { display: grid; gap: 14px; padding: 20px; border-radius: 16px; background: var(--ink-2); border: 1px solid var(--line); }
.step.two { grid-template-columns: minmax(0, 1fr) minmax(0, 1.25fr); gap: 28px; }
.col { display: flex; flex-direction: column; gap: 10px; min-width: 0; }
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

.error { margin: 0; padding: 10px 14px; border-radius: 10px; border: 1px solid var(--danger); color: var(--danger); }
.create { display: flex; align-items: center; justify-content: flex-end; gap: 18px; }
.back { margin-right: auto; }
.steps { display: flex; flex-wrap: wrap; gap: 8px 22px; margin: 0; padding: 0; list-style: none; }
.steps li { display: flex; align-items: center; gap: 8px; font-size: 0.85rem; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; color: var(--text-faint); }
.steps .n { background: var(--ink-5); }
.steps .current { color: var(--title); }
.steps .current .n, .steps .done .n { background: var(--crimson); }

@media (max-width: 860px) {
  .step.two { grid-template-columns: minmax(0, 1fr); }
}
</style>
