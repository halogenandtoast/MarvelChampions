<script lang="ts" setup>
import { withDefaults, defineProps, defineEmits, computed } from 'vue'
import { PlayerCard } from '@/marvel/types/PlayerCard'
import { Game } from '@/marvel/types/Game'
import * as MarvelGame from '@/marvel/types/Game'

const props = withDefaults(defineProps<{
  card: PlayerCard
  identityId: string
  game: Game
  sideways: boolean
}>(), { sideways: false })

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const cardImage = computed(() => `/img/marvel/cards/${props.card.cardDef.cdCardCode}.jpg`)
const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const playCardAction = computed(() => {
  return choices
    .value
    .findIndex((c) => c.tag === 'PlayCard' && c.contents.cardId === props.card.cardId)
})

const targetAbility = computed(() => {
  return choices
    .value
    .findIndex((c) => c.tag === 'TargetLabel' && c.target.contents === props.card.cardId)
})

const activeAbility = computed(() => {
  if (targetAbility.value !== -1) {
    return targetAbility.value
  }

  return playCardAction.value
})

const payWithCardAction = computed(() => {
  return choices
    .value
    .findIndex((c) => c.tag === 'PayWithCard' && c.contents.cardId === props.card.cardId)
})
</script>

<template>
  <div class="card">
    <img :src="cardImage" alt="card" :class="{ sideways: sideways, active: activeAbility !== -1 }" @click="emit('choose', activeAbility)" data-role="card" />
    <button v-if="payWithCardAction !== -1" @click="emit('choose', payWithCardAction)">Pay</button>
  </div>
</template>

<style scoped lang="scss">
.card {
  position: relative;
  display: flex;
  flex-direction: column;
  img {
    width: 100px;
    margin: 2px;
    border-radius: 10px;
    &.sideways {
      width: 140px;
    }
  }

}

.active {
  border: 2px solid #FF00FF;
}
</style>
