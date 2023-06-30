<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { MainScheme } from '@/marvel/types/MainScheme'
import Card from '@/marvel/components/Card.vue'

const props = defineProps<{
  game: Game
  identityId: string
  mainScheme: MainScheme
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({
  cardId: props.mainScheme.mainSchemeId,
  cardDef: props.mainScheme.mainSchemeCardDef
}))
</script>

<template>
  <div class="mainScheme">
    <Card
      :card="card"
      :game="game"
      :identityId="identityId"
      :sideways="true"
      class="scenario-card"
      @choose="emit('choose', $event)"
    />
    <div class="threat">threat: {{mainScheme.mainSchemeThreat}}</div>
    <div v-if="game.scenario.scenarioAccelerationTokens > 0">Acceleration Tokens: {{game.scenario.scenarioAccelerationTokens}}</div>
  </div>
</template>
