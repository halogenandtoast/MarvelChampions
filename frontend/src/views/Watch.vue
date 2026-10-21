<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchSpectatableGames } from '../api'
import GameRow from '../components/GameRow.vue'
import type { GameSummary } from '../server'

const { t } = useI18n()
const games = ref<GameSummary[] | null>(null)
const error = ref<string | null>(null)

fetchSpectatableGames()
  .then((result) => (games.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))
</script>

<template>
  <div class="page-container">
    <div class="page-content watch">
      <h1 class="page-title">{{ $t('watch.title') }}</h1>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <div v-else-if="games?.length === 0" class="box"><p>{{ $t('watch.none') }}</p></div>
      <div class="games"><GameRow v-for="game in games ?? []" :key="game.id" :game="game" spectate /></div>
    </div>
  </div>
</template>

<style scoped>
.games { display: grid; gap: 10px; }
</style>
