<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { SideScheme } from '@/marvel/types/SideScheme'
import Card from '@/marvel/components/Card.vue'

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
</script>

<template>
  <div class="sideScheme">
    <Card
      :card="card"
      :game="game"
      :identityId="identityId"
      :sideways="true"
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
