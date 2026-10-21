<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchAdminDashboard, type AdminDashboard } from '../api'
import GameRow from '../components/GameRow.vue'

const { t } = useI18n()
const dashboard = ref<AdminDashboard | null>(null)
const error = ref<string | null>(null)

fetchAdminDashboard()
  .then((result) => (dashboard.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))
</script>

<template>
  <div class="page-container">
    <div class="page-content admin">
      <h1 class="page-title">{{ $t('nav.admin') }}</h1>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <template v-if="dashboard">
        <div class="stats">
          <div class="box stat"><span class="value">{{ dashboard.users }}</span>{{ $t('admin.users') }}</div>
          <div class="box stat"><span class="value">{{ dashboard.activeUsers }}</span>{{ $t('admin.activeUsers') }}</div>
          <div class="box stat"><span class="value">{{ dashboard.games }}</span>{{ $t('admin.games') }}</div>
        </div>
        <h3>{{ $t('admin.recentGames') }}</h3>
        <!-- an admin may watch any game -->
        <div class="games"><GameRow v-for="game in dashboard.recentGames" :key="game.id" :game="game" spectate /></div>
      </template>
    </div>
  </div>
</template>

<style scoped>
h3 { color: var(--text-muted); text-transform: uppercase; margin: 24px 0 10px; font-size: 1.3rem; }
.games { display: grid; gap: 10px; }
.stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 10px; }
.stat { display: flex; flex-direction: column; gap: 4px; color: var(--text-muted); font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.08em; }
.stat .value { color: var(--title); font-family: var(--font-display); font-size: 2.4rem; font-weight: 800; letter-spacing: 0; line-height: 1; }
</style>
