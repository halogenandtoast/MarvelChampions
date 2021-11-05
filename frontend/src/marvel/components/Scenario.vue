<template>
  <div>
    <img :src="scenarioImg" alt="Scenario" />
    <Player
      v-for="player in game.players"
      :key="player.id"
      :player="player"
      :game="game"
    />
  </div>
</template>

<script lang="ts">

import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import Player from '@/marvel/components/Player.vue'

export default defineComponent({
  props: { game: { type: Object as () => Game, required: true } },
  components: { Player },
  setup(props) {
    const scenarioImg = computed(() => `/img/marvel/cards/${props.game.scenario.contents.scenarioId}.jpg`)
    return { scenarioImg }
  }
})
</script>
