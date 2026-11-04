<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { Plus, Upload } from 'lucide-vue-next'
import { deleteCampaign, deleteGame, fetchCampaigns, fetchGames, fetchSpectatableGames, importGame } from '../api'
import GameRow from '../components/GameRow.vue'
import CampaignBar from '../components/CampaignBar.vue'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import type { CampaignView, GameSummary } from '../server'
import { useServerText } from '../i18n'
import { currentUser } from '../auth'

const tr = useServerText()
type Tab = 'active' | 'finished'

const { t } = useI18n()
const router = useRouter()
const games = ref<GameSummary[] | null>(null)
const watchable = ref<GameSummary[]>([])
const error = ref<string | null>(null)

const activeGames = computed(() => (games.value ?? []).filter((g) => g.outcome === null))
const finishedGames = computed(() => (games.value ?? []).filter((g) => g.outcome !== null))

// null until the player picks a tab, so they land on whichever has something in it
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => chosenTab.value ?? (activeGames.value.length === 0 && finishedGames.value.length > 0 ? 'finished' : 'active'))
const tabOptions = computed(() => [
  { value: 'active' as Tab, label: t('home.activeGames'), count: activeGames.value.length },
  { value: 'finished' as Tab, label: t('home.finishedGames'), count: finishedGames.value.length },
])
const shown = computed(() => (tab.value === 'active' ? activeGames.value : finishedGames.value))

fetchGames()
  .then((result) => (games.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))
fetchSpectatableGames()
  .then((result) => (watchable.value = result))
  .catch(() => {})

const campaigns = ref<CampaignView[]>([])
fetchCampaigns()
  .then((result) => (campaigns.value = result))
  .catch(() => {})
const campaignsById = computed(() => new Map(campaigns.value.map(c => [c.id, c])))
const campaignsByGame = computed(() => new Map(campaigns.value.filter(c => c.currentGame).map(c => [c.currentGame!, c])))
const campaignFor = (game: GameSummary) => (game.campaignId ? campaignsById.value.get(game.campaignId) : undefined) ?? campaignsByGame.value.get(game.id)
const unlistedCampaigns = computed(() => {
  const listed = new Set(shown.value.map(game => campaignFor(game)?.id))
  return campaigns.value.filter(c => !listed.has(c.id))
})

const removeGame = (game: GameSummary) =>
  deleteGame(game.id).then(() => {
    games.value = (games.value ?? []).filter((g) => g.id !== game.id)
  })

const removeCampaign = async (campaign: CampaignView) => {
  const linkedGames = new Set((games.value ?? []).filter(game => campaignFor(game)?.id === campaign.id).map(game => game.id))
  try {
    const response = await deleteCampaign(campaign.id)
    if (!response.ok) throw new Error(await response.text())
    campaigns.value = campaigns.value.filter(c => c.id !== campaign.id)
    games.value = (games.value ?? []).filter(game => !linkedGames.has(game.id))
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('campaign.couldNotDelete')
  }
}

const showImportGame = ref(false)
const importFile = ref<File | null>(null)
const importing = ref(false)
const importError = ref<string | null>(null)

const chooseFile = (event: Event) => {
  importFile.value = (event.target as HTMLInputElement).files?.[0] ?? null
  importError.value = null
}

const submitImport = async () => {
  if (!importFile.value) return
  importing.value = true
  importError.value = null
  try {
    const { id } = await importGame(importFile.value)
    await router.push(`/games/${id}`)
  } catch (e) {
    importError.value = e instanceof Error ? e.message : t('home.couldNotLoadFile')
  } finally {
    importing.value = false
  }
}
</script>

<template>
  <div class="page-container">
    <div class="page-content home">
      <header class="head">
        <h1 class="page-title">{{ $t('home.yourGames') }}</h1>
        <div class="actions">
          <button class="btn ghost" type="button" @click="showImportGame = !showImportGame"><Upload aria-hidden="true" />{{ $t('home.loadGame') }}</button>
          <RouterLink to="/new-game" class="btn primary"><Plus aria-hidden="true" />{{ $t('newGame.title') }}</RouterLink>
        </div>
      </header>

      <Transition name="fade">
        <div v-if="showImportGame" class="load-game box">
          <label class="file-picker">
            <input type="file" accept="application/json,.json" @change="chooseFile" />
            <span>{{ importFile ? importFile.name : $t('home.chooseFile') }}</span>
          </label>
          <div class="load-actions">
            <button class="btn ghost" type="button" @click="showImportGame = false">{{ $t('common.cancel') }}</button>
            <button class="btn primary" type="button" :disabled="!importFile || importing" @click="submitImport">{{ importing ? $t('home.loading') : $t('home.loadGame') }}</button>
          </div>
          <p v-if="importError" class="import-error">{{ tr(importError) }}</p>
        </div>
      </Transition>

      <SegmentedToggle class="tabs" :model-value="tab" :options="tabOptions" :label="$t('home.games')" @update:model-value="chosenTab = $event as Tab" />

      <div v-if="error" class="box error"><p>{{ tr(error) }}</p></div>
      <div v-else-if="games && shown.length === 0" class="box empty">
        <p>{{ $t(tab === 'active' ? 'home.noActiveGames' : 'home.noFinishedGames') }}</p>
      </div>
      <div class="games">
        <GameRow v-for="game in shown" :key="game.id" :game="game" :campaign="campaignFor(game)" :delete-campaign="campaignFor(game)?.table.owner === currentUser?.id ? () => removeCampaign(campaignFor(game)!) : undefined" :delete-game="() => removeGame(game)" />
      </div>

      <section v-if="unlistedCampaigns.length" class="watchable">
        <header class="sub-head">
          <h2>{{ $t('campaign.campaigns') }}</h2>
        </header>
        <div class="games">
          <CampaignBar v-for="c in unlistedCampaigns" :key="c.id" :campaign="c" standalone :delete-campaign="c.table.owner === currentUser?.id ? () => removeCampaign(c) : undefined" />
        </div>
      </section>

      <section v-if="watchable.length" class="watchable">
        <header class="sub-head">
          <h2>{{ $t('home.spectate') }}</h2>
          <RouterLink to="/watch">{{ $t('home.watchAll') }}</RouterLink>
        </header>
        <div class="games">
          <GameRow v-for="game in watchable.slice(0, 3)" :key="game.id" :game="game" spectate />
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
.head { display: flex; align-items: center; gap: 12px; margin-bottom: 16px; flex-wrap: wrap; }
.head .page-title { margin: 0 auto 0 0; }
.actions { display: flex; gap: 8px; }
.tabs { width: 18rem; margin-bottom: 14px; }
.games { display: grid; gap: 10px; }
.empty { display: flex; align-items: center; justify-content: space-between; gap: 16px; color: var(--text-muted); padding: 22px; }
.load-game { display: grid; gap: 10px; margin-bottom: 16px; }
.load-actions { display: flex; justify-content: flex-end; gap: 8px; }
.file-picker { display: block; border: 1px dashed var(--line-strong); border-radius: 10px; padding: 20px; text-align: center; color: var(--text-muted); cursor: pointer; }
.file-picker:hover { border-color: var(--crimson); color: var(--text); }
.file-picker input { display: none; }
.import-error { margin: 0; color: var(--danger); }
.watchable { margin-top: 36px; }
.sub-head { display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px; }
.sub-head h2 { margin: 0; font-size: 1.4rem; text-transform: uppercase; color: var(--text-muted); }
.fade-enter-active, .fade-leave-active { transition: opacity 0.2s; }
.fade-enter-from, .fade-leave-to { opacity: 0; }
</style>
