<template>
  <div id="game" v-if="ready">
    <div v-if="socketError" class="socketWarning">
       <p>Your game is out of sync, trying to reconnect...</p>
    </div>
    <div class="game">
      <Scenario
        :game="game"
        :identityId="identityId"
        @choose="choose"
        @update="update"
      />
      <div v-if="game.gameOver">
        <p>Game over</p>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue'
import * as Marvel from '@/marvel/types/Game'
import { fetchGame } from '@/marvel/api'
import Scenario from '@/marvel/components/Scenario.vue'

export default defineComponent({
  components: { Scenario },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const ready = ref(false)
    const game = ref<Marvel.Game | null>(null)
    const identityId = ref<string | null>(null)
    fetchGame(props.gameId).then(({ game: newGame }) => {
      game.value = newGame;
      ready.value = true
    });
    async function choose(idx: number) {
      console.log(idx)
    }
    /* eslint-disable @typescript-eslint/no-explicit-any */
    async function update(state: Marvel.Game) {
      game.value = state;
    }

    return { ready, game, identityId, choose, update }
  }
})
</script>
