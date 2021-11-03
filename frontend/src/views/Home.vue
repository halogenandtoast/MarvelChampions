<template>
  <div class="home">
    <router-link to="/games/new" custom v-slot="{ navigate }">
      <button @click="navigate">New Game</button>
    </router-link>

    <h2>Active Games</h2>
    <div v-for="game in games" class="game" :key="game.id">
      <div class="game-details">
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { fetchGames } from '@/marvel/api';
import { Game } from '@/marvel/types/Game';
import { User } from '@/types';
export default defineComponent({
  setup() {
    const store = useStore()
    const currentUser = computed<User | null>(() => store.getters.currentUser)
    const games: Ref<Game[]> = ref([])
    fetchGames().then((result) => games.value = result)
    return { currentUser, games }
  }
})
</script>
