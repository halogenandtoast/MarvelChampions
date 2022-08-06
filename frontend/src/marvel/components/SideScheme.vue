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
  cardId: props.sideScheme.sideSchemeId,
  cardDef: props.sideScheme.sideSchemeCardDef
}))

const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const activeAbility = computed(() =>
  choices.value.findIndex((choice) => {
    if (choice.tag !== 'TargetLabel') {
      return false
    }

    const { contents } = choice.target
      if (typeof contents === "string") {
        return contents == props.sideScheme.sideSchemeId
      }

      switch (contents.tag) {
        case 'SchemeSideSchemeId':
          return contents.contents === props.sideScheme.sideSchemeId
        default:
          return false
      }
  }))
</script>

<template>
  <div class="sideScheme">
    <Card
      :card="card"
      :game="game"
      :identityId="identityId"
      :class="{ active: activeAbility !== -1 }"
      @click="emit('choose', activeAbility)"
      @choose="emit('choose', $event)"
    />
    <div class="threat">threat: {{sideScheme.sideSchemeThreat}}</div>
  </div>
</template>


<style scoped lang="scss">
.sideScheme {
  display: inline-block;
}
</style>
