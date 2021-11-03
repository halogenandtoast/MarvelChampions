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
import { defineComponent, ref, onUnmounted } from 'vue'
import * as Marvel from '@/marvel/types/Game'
import { fetchGame } from '@/marvel/api'
import Scenario from '@/marvel/components/Scenario.vue'
import { onBeforeRouteLeave } from 'vue-router'
export default defineComponent({
  components: { Scenario },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const ready = ref(false)
    const socketError = ref(false)
    const socket = ref<WebSocket | null>(null)
    const game = ref<Marvel.Game | null>(null)
    const identityId = ref<string | null>(null)
    function connect() {
      const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
      socket.value = new WebSocket(`${baseURL}/api/v1/arkham/games/${props.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      socket.value.addEventListener('open', () => {
        ready.value = true;
        socketError.value = false;
      });
      socket.value.addEventListener('error', () => {
        socketError.value = true;
        if (socket.value) {
          socket.value.close();
          socket.value = null;
        }
      });
      socket.value.addEventListener('close', () => {
        socketError.value = true;
        socket.value = null;
        setTimeout(() => connect(), 1000);
      });
    }
    fetchGame(props.gameId).then(({ game: newGame }) => {
      game.value = newGame;
      connect();
    });
    async function choose(idx: number) {
      console.log(idx)
    }
    /* eslint-disable @typescript-eslint/no-explicit-any */
    async function update(state: Marvel.Game) {
      game.value = state;
    }
    onBeforeRouteLeave(() => { if (socket.value) { socket.value.close(); socket.value = null; } })
    onUnmounted(() => { if (socket.value) { socket.value.close(); socket.value = null; }})

    return { socketError, ready, game, identityId, choose, update }
  }
})
</script>
