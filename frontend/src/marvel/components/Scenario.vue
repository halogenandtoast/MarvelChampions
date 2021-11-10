<template>
  <div>
    <img :src="scenarioImg" alt="Scenario" />
    <div>{{game.scenario.contents.scenarioThreat}}</div>
    <Villain
      v-for="villain in game.villains"
      :key="villain.contents.villainId"
      :villain="villain"
      :identityId="identityId"
      :game="game"
      @choose="$emit('choose', $event)"
    />
    <Player
      v-for="player in game.players"
      :key="player.id"
      :player="player"
      :identityId="identityId"
      :game="game"
      @choose="$emit('choose', $event)"
    />
  </div>
</template>

<script lang="ts">

import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import Player from '@/marvel/components/Player.vue'
import Villain from '@/marvel/components/Villain.vue'

export default defineComponent({
  props: { game: { type: Object as () => Game, required: true }, identityId: { type: String, required: true } },
  components: { Player, Villain },
  setup(props) {
    const scenarioImg = computed(() => `/img/marvel/cards/${props.game.scenario.contents.scenarioId}.jpg`)
    return { scenarioImg }
  }
})
</script>
