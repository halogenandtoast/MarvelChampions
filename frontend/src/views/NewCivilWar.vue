<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { createGame, fetchScenarios } from '../api'
import { currentUser } from '../auth'
import { useServerText } from '../i18n'
import { useCards } from '../cards'
import { useScenarios } from '../scenarios'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import type { CivilWarTeamScenario, ScenarioList } from '../server'

const { t } = useI18n()
const tr = useServerText()
const router = useRouter()
const cards = useCards()
const { modularName } = useScenarios(ref([]))
const options = ref<ScenarioList['civilWar'] | null>(null)
const teams = ref<Record<'registration' | 'resistance', CivilWarTeamScenario> | null>(null)
const factions = ['registration', 'resistance'] as const
const seats = ref('2')
const difficulty = ref<'Normal' | 'Expert'>('Normal')
const name = ref('')
const spectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const error = ref<string | null>(null)
const creating = ref(false)
const cardName = (code: string) => cards.card(code)?.name ?? code
const copy = (scenario: CivilWarTeamScenario) => ({ ...scenario, modulars: [...scenario.modulars], mainSchemes: [...scenario.mainSchemes] })
onMounted(async () => {
  try {
    options.value = (await fetchScenarios()).civilWar
    teams.value = { registration: copy(options.value.registration.presets[0]!), resistance: copy(options.value.resistance.presets[0]!) }
  } catch (e) { error.value = e instanceof Error ? e.message : String(e) }
})
const pickLeader = (faction: typeof factions[number], event: Event) => {
  const preset = options.value?.[faction].presets.find((p) => p.leader === (event.target as HTMLSelectElement).value)
  if (preset && teams.value) teams.value[faction] = copy(preset)
}
const valid = computed(() => teams.value && factions.every((f) => [3, 4].includes(teams.value![f].modulars.length)))
const create = async () => {
  if (!valid.value || !teams.value || creating.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createGame({ name: name.value.trim() || t('civilWar.title'), decks: [], seats: Number(seats.value), allowSpectators: spectators.value,
      scenario: { scenarioCode: teams.value.registration.leader, modulars: teams.value.registration.modulars, difficulty: difficulty.value, mainSchemes: teams.value.registration.mainSchemes, competition: teams.value } })
    await router.push(`/games/${id}`)
  } catch (e) { error.value = e instanceof Error ? e.message : String(e); creating.value = false }
}
</script>

<template>
  <div class="page-container">
  <main class="page-content civil-war">
    <RouterLink to="/new-game">{{ t('common.back') }}</RouterLink>
    <header><h1 class="page-title">{{ t('civilWar.title') }}</h1><p>{{ t('civilWar.intro') }}</p></header>
    <p v-if="error" role="alert">{{ tr(error) }}</p>
    <p v-if="!teams && !error">{{ t('civilWar.loading') }}</p>
    <form v-if="teams && options" @submit.prevent="create">
      <div class="settings">
        <label>{{ t('civilWar.seats') }}<SegmentedToggle v-model="seats" :label="t('civilWar.seats')" :options="[{value: '2', label: t('civilWar.duel')}, {value: '4', label: t('civilWar.teams')}]" /></label>
        <label>{{ t('newGame.difficulty') }}<select v-model="difficulty" class="input"><option value="Normal">{{ t('difficulty.Normal') }}</option><option value="Expert">{{ t('difficulty.Expert') }}</option></select></label>
      </div>
      <div class="factions">
        <fieldset v-for="faction in factions" :key="faction">
          <legend>{{ t(`civilWar.${faction}`) }}</legend>
          <label>{{ t('civilWar.leader') }}<select class="input" :value="teams[faction].leader" @change="pickLeader(faction, $event)"><option v-for="preset in options[faction].presets" :key="preset.leader" :value="preset.leader">{{ cardName(preset.leader) }}</option></select></label>
          <label>{{ t('civilWar.firstScheme') }}<select v-model="teams[faction].mainSchemes[0]" class="input"><option v-for="code in options[faction].firstSchemes" :key="code" :value="code">{{ cardName(code) }}</option></select></label>
          <label>{{ t('civilWar.secondScheme') }}<select v-model="teams[faction].mainSchemes[1]" class="input"><option v-for="code in options[faction].secondSchemes" :key="code" :value="code">{{ cardName(code) }}</option></select></label>
          <h2>{{ t('civilWar.modulars') }} · {{ teams[faction].modulars.length }}/4</h2>
          <p>{{ t('civilWar.modularHint') }}</p>
          <label v-for="set in options[faction].modulars" :key="set" class="modular"><input v-model="teams[faction].modulars" type="checkbox" :value="set" :disabled="teams[faction].modulars.length >= 4 && !teams[faction].modulars.includes(set)" />{{ modularName(set) }}</label>
        </fieldset>
      </div>
      <label>{{ t('civilWar.name') }}<input v-model="name" class="input" :placeholder="t('civilWar.title')" /></label>
      <label class="modular"><input v-model="spectators" type="checkbox" />{{ t('civilWar.spectators') }}</label>
      <button class="btn primary" type="submit" :disabled="!valid || creating">{{ t('civilWar.create') }}</button>
    </form>
  </main>
  </div>
</template>

<style scoped>
.civil-war, form { display: grid; gap: 24px; }
header p { max-width: 65ch; }
.factions, .settings { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 24px; }
fieldset { min-width: 0; padding: 24px; border: 1px solid var(--line); border-radius: 12px; }
legend { font-size: 1.25rem; font-weight: 700; padding: 0 8px; }
label { display: grid; gap: 8px; margin-bottom: 16px; }
.modular { display: flex; gap: 10px; align-items: center; }
h2 { font-size: 1rem; margin-top: 24px; }
select, input:not([type='checkbox']) { width: 100%; }
@media (max-width: 720px) { .factions, .settings { grid-template-columns: 1fr; } }
</style>
