<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { MainScheme } from '@/marvel/types/MainScheme'
import Card from '@/marvel/components/Card.vue'
import * as MarvelGame from '@/marvel/types/Game'

const props = defineProps<{
  game: Game
  identityId: string
  mainScheme: MainScheme
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({
  cardId: props.mainScheme.contents.mainSchemeId,
  cardDef: props.mainScheme.contents.mainSchemeCardDef
}))

const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const activeAbility = computed(() => {
  return choices.
    value.
    findIndex((choice) => {
      if (choice.tag !== 'TargetLabel') {
        return false
      }

      const { contents } = choice.target
      if (typeof contents === "string") {
        return contents == props.game.scenario.contents.scenarioId
      }

      switch (contents.tag) {
        case 'SchemeMainSchemeId':
          return contents.contents === props.game.scenario.contents.scenarioId
        default:
          return false
      }
    })
})

</script>

<template>
  <div class="mainScheme">
    <Card
      :card="card"
      :game="game"
      :identityId="identityId"
      :class="{ active: activeAbility !== -1 }"
      class="scenario-card"
      @click="emit('choose', activeAbility)"
      @choose="emit('choose', $event)"
    />
    <div class="threat">threat: {{mainScheme.contents.mainSchemeThreat}}</div>
    <div v-if="game.scenario.contents.scenarioAccelerationTokens > 0">Acceleration Tokens: {{game.scenario.contents.scenarioAccelerationTokens}}</div>
  </div>
</template>
