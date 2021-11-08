<template>
  <div class="card">
    <img :src="cardImage" alt="card" :class="{ active: playCardAction !== -1 }" @click="$emit('choose', playCardAction)" />
    <button v-if="payWithCardAction !== -1" @click="$emit('choose', payWithCardAction)">Pay</button>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { PlayerCard } from '@/marvel/types/Identity'
import { Game } from '@/marvel/types/Game'
import * as MarvelGame from '@/marvel/types/Game'

export default defineComponent({
  props: {
    card: { type: Object as () => PlayerCard, required: true },
    identityId: { type: String, required: true },
    game: { type: Object as () => Game, required: true }
  },
  setup(props) {
    const cardImage = computed(() => `/img/marvel/cards/${props.card.pcCardDef.cdCardCode}.jpg`)
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const playCardAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'PlayCard' && c.contents.pcCardId === props.card.pcCardId)
    })

    const payWithCardAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'PayWithCard' && c.contents.pcCardId === props.card.pcCardId)
    })

    return { cardImage, playCardAction, payWithCardAction }
  }
})
</script>

<style scoped lang="scss">
.card {
  display: flex;
  flex-direction: column;
  img {
    width: 150px;
    margin: 2px;
    border-radius: 10px;
  }
}

.active {
  border: 2px solid #FF00FF;
}
</style>
