<script setup lang="ts">
import { useI18n } from 'vue-i18n'
import type { MissionView } from '../server'
import BoardCard from './BoardCard.vue'
import Meter from './Meter.vue'

defineProps<{ mission: MissionView }>()
const { t } = useI18n()
</script>

<template>
  <section class="mission-area" :aria-label="t('game.missionArea')">
    <header>
      <h2>{{ t('game.missionArea') }}</h2>
      <span>{{ mission.finished ? t(mission.succeeded ? 'game.missionSucceeded' : 'game.missionFailed') : t('game.missionAttempts', { count: mission.attempts }) }}</span>
    </header>
    <div class="mission-cards">
      <BoardCard v-if="mission.scheme" :id="mission.scheme.id" :code="mission.scheme.code" :name="mission.scheme.name" :threat="mission.scheme.threat" :counters="mission.attempts" />
      <div v-for="minion in mission.minions" :key="minion.id">
        <BoardCard :id="minion.id" :code="minion.code" :name="minion.name" :damage="minion.damage" :tough="minion.tough" :held="minion.facedownCards" :attached="[...(minion.attachments ?? []), ...(minion.upgrades ?? [])]" />
        <Meter :label="t('game.hp')" :value="Math.max(0, minion.hitPoints - minion.damage)" :max="minion.hitPoints" tone="villain" />
      </div>
      <BoardCard v-for="ally in mission.allies" :id="ally.id" :key="ally.id" :code="ally.code" :name="ally.name" :damage="ally.damage" :exhausted="ally.exhausted" :attached="[...(ally.attachments ?? []), ...(ally.upgrades ?? [])]" />
    </div>
    <div v-if="mission.assignments.length" class="assignments">
      <span v-for="[ally, card] in mission.assignments" :key="ally">{{ mission.allies.find(a => a.id === ally)?.name }}: {{ card.name }}</span>
    </div>
  </section>
</template>

<style scoped>
.mission-area { border-top: 1px solid var(--border, #7775); padding: 1rem; }
header, .mission-cards, .assignments { display: flex; gap: 1rem; flex-wrap: wrap; align-items: start; }
header { justify-content: space-between; margin-bottom: 1rem; }
h2 { margin: 0; font: inherit; font-weight: 600; }
.assignments { margin-top: 1rem; }
</style>
