<template>
  <div class="home">
    <router-link to="/games/new" custom v-slot="{ navigate }">
      <button @click="navigate">New Game</button>
    </router-link>

    <h2>Active Games</h2>
    <div v-for="game in games" class="game" :key="game.id">
      <div class="game-details">
        {{game}}
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        <div class="game-delete">
          <a href="#delete" @click.prevent="deleteId = game.id"><font-awesome-icon icon="trash" /></a>
        </div>
      </div>
    </div>

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this game?"
      :yes="deleteGameEvent"
      :no="() => deleteId = null"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { fetchGames, deleteGame } from '@/marvel/api';
import { Game } from '@/marvel/types/Game';
import { User } from '@/types';
import Prompt from '@/components/Prompt.vue';

export default defineComponent({
  components: { Prompt },
  setup() {
    const store = useStore()
    const deleteId = ref<string | null>(null)
    const currentUser = computed<User | null>(() => store.getters.currentUser)
    const games: Ref<Game[]> = ref([])
    fetchGames().then((result) => games.value = result)

    async function deleteGameEvent() {
      const { value } = deleteId
      if (value) {
        deleteGame(value).then(() => {
          games.value = games.value.filter((game) => game.id !== value);
          deleteId.value = null;
        });
      }
    }

    return { currentUser, games, deleteId, deleteGameEvent }
  }
})
</script>
