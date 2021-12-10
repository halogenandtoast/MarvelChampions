<script lang="ts" setup>
import { ref, computed } from 'vue';
import { useRouter } from 'vue-router';
import * as Marvel from '@/marvel/types/Deck';
import { fetchDecks, newGame } from '@/marvel/api';

const scenarios = [
  {
    id: '01097',
    name: 'Rhino - The Break-In!',
  },
]

const router = useRouter()
const decks = ref<Marvel.Deck[]>([])
const ready = ref(false)
const playerCount = ref(1)
const deckIds = ref<(string | null)[]>([null, null, null, null])
const selectedScenario = ref('01097')
const gameName = ref<string | null>(null)
const multiplayerVariant = ref('Solo')
fetchDecks().then((result) => {
  decks.value = result;
  ready.value = true;
})

const disabled = computed(() => {
  return !deckIds.value[0]
})

const defaultGameName = computed(() => {
  const scenario = scenarios.find((c) => c.id === selectedScenario.value);
  if (scenario) {
    return `${scenario.name}`;
  }
  return '';
})

const currentGameName = computed(() => {
  if (gameName.value !== '' && gameName.value !== null) {
    return gameName.value;
  }
  return defaultGameName.value;
})

async function start() {
  const mscenario = scenarios.find((scenario) => scenario.id === selectedScenario.value);
  if (mscenario && currentGameName.value) {
    newGame(
      deckIds.value,
      playerCount.value,
      mscenario.id,
      currentGameName.value,
      multiplayerVariant.value,
    ).then((game) => router.push(`/games/${game.id}`));
  }
}
</script>

<template>
  <div v-if="ready" class="container">
    <div v-if="decks.length == 0">
      No decks, please add one first here <router-link to="/decks">here</router-link>
    </div>
    <div v-else>
      <header>
        <router-link to="/" class="back-link">‹</router-link>
        <h2>New Game</h2>
      </header>
      <form id="new-game" @submit.prevent="start">
        <p>Deck</p>
        <select v-model="deckIds[0]">
          <option disabled :value="null">-- Select a Deck--</option>
          <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
        </select>

        <div>
          <p>Game Name</p>
          <input type="text" v-model="gameName" :placeholder="currentGameName" />
        </div>

        <button type="submit" :disabled="disabled">Create</button>
      </form>
    </div>
  </div>
</template>
