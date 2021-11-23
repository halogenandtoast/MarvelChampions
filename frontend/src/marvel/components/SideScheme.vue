<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { SideScheme } from '@/marvel/types/SideScheme'
import Card from '@/marvel/components/Card.vue'
import * as MarvelGame from '@/marvel/types/Game'

const props = defineProps<{
  game: Game
  identityId: string
  sideScheme: SideScheme
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({
  cardId: props.sideScheme.contents.sideSchemeId,
  cardDef: props.sideScheme.contents.sideSchemeCardDef
}))

const choices = MarvelGame.choices(props.game, props.identityId)

const activeAbility =
  choices.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.sideScheme.contents.sideSchemeId)
</script>

<template>
  <div class="sideScheme">
    <Card
      :card="card"
      :game="game"
      :identityId="identityId"
      :class="{ active: activeAbility !== -1 }"
      @choose="emit('choose', $event)"
    />
    <div class="threat">threat: {{sideScheme.contents.sideSchemeThreat}}</div>
  </div>
</template>


<style scoped lang="scss">
.sideScheme {
  display: inline-block;
}
</style>
