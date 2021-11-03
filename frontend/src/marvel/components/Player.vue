<template>
  <div>
    <img :src="playerImg" alt="player" />
  </div>
</template>

<script lang="ts">

import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { Identity } from '@/marvel/types/Identity'

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    player: { type: Object as () => Identity, required: true }
  },
  setup(props) {
    const playerCardCode = computed(() => {
      const side = props.player.sides[props.player.side]
      switch(side.tag) {
        case 'AlterEgoSide':
          return side.contents.contents.alterEgoCardDef.cdCardCode
        case 'HeroSide':
          return side.contents.contents.heroCardDef.cdCardCode
        default:
          return null
      }
    })

    const playerImg = computed(() => `/img/marvel/cards/${playerCardCode.value}.jpg`)

    return { playerImg }
  }
})
</script>
