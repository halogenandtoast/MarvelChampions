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
import { fetchGame, updateGame } from '@/marvel/api'
import Scenario from '@/marvel/components/Scenario.vue'

export default defineComponent({
  components: { Scenario },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const ready = ref(false)
    const game = ref<Marvel.Game | null>(null)
    const identityId = ref<string | null>(null)
    const socketError = ref(false)
    const socket = ref<WebSocket | null>(null)
    // const solo = ref(true)

    const spectate = false

    function connect() {
          const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`
          const spectatePrefix = spectate ? "/spectate" : ""
          socket.value = new WebSocket(`${baseURL}/api/v1/marvel/games/${props.gameId}${spectatePrefix}`.replace(/https/, 'wss').replace(/http/, 'ws'))
          socket.value.addEventListener('open', () => {
            ready.value = true
            socketError.value = false
          });
          socket.value.addEventListener('message', (event: MessageEvent) => {
            const data = JSON.parse(event.data)
            // if (data.tag === "GameMessage") {
            //   gameLog.value = Object.freeze([...gameLog.value, data.contents])
            // }
            if (data.tag === "GameUpdate") {
              Marvel.gameDecoder.decodeToPromise(data.contents)
                .then((updatedGame) => {
                  game.value = updatedGame
                  // if (solo.value) {
                  //   if (Object.keys(game.value.question).length == 1) {
                  //     identityId.value = Object.keys(game.value.question)[0]
                  //   } else if (game.value.activeInvestigatorId !== identityId.value) {
                  //     identityId.value = Object.keys(game.value.question)[0]
                  //   }
                  // }
                })
            }
          })
          socket.value.addEventListener('error', () => {
            socketError.value = true
            if (socket.value) {
              socket.value.close()
              socket.value = null
            }
          })
          socket.value.addEventListener('close', () => {
            socketError.value = true
            socket.value = null
            setTimeout(() => connect(), 1000)
          })
        }

    fetchGame(props.gameId).then(({ game: newGame }) => {
      game.value = newGame
      connect()
    });

    async function choose(idx: number) {
      if (idx !== -1 && game.value) {
        updateGame(props.gameId, idx, identityId.value)
      }
    }
    /* eslint-disable @typescript-eslint/no-explicit-any */
    async function update(state: Marvel.Game) {
      game.value = state;
    }

    return { ready, game, identityId, choose, update }
  }
})
</script>
