<script lang="ts" setup>

import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import Player from '@/marvel/components/Player.vue'
import Villain from '@/marvel/components/Villain.vue'
import MainScheme from '@/marvel/components/MainScheme.vue'
import SideScheme from '@/marvel/components/SideScheme.vue'

const props = defineProps<{
  game: Game
  identityId: string
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const topOfDiscard = computed(() => props.game.scenario.contents.scenarioDiscard[0])

const focusedCards = computed(() => props.game.focusedCards)
</script>

<template>
  <div class="scenario">
    <div class="encounter">
      <Villain
        v-for="villain in game.villains"
        :key="villain.contents.villainId"
        :villain="villain"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />
      <Card v-if="topOfDiscard" :card="topOfDiscard" :game="game" :identityId="identityId" class="discard" />
      <img src="/img/marvel/encounter-back.png" alt="deck" width="150" height="209" class="deck" />

      <MainScheme
        v-for="mainScheme in game.mainSchemes"
        :key="mainScheme.contents.mainSchemeId"
        :mainScheme="mainScheme"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />

      <SideScheme
        v-for="sideScheme in game.sideSchemes"
        :key="sideScheme.contents.sideSchemeId"
        :sideScheme="sideScheme"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />
    </div>
    <div v-if="focusedCards.length > 0" class="focused">
      <Card v-for="card in focusedCards" :key="card.contents.cardId" :card="card.contents" :game="game" :identityId="identityId" @choose="emit('choose', $event)" />
    </div>
    <Player
      v-for="player in game.players"
      :key="player.id"
      :player="player"
      :identityId="identityId"
      :game="game"
      @choose="emit('choose', $event)"
    />
  </div>
</template>

<style scoped lang="scss">
.scenario {
  display: flex;
  flex-flow: column;
  height: 100%;
  border-radius: 10px;
}

.active {
  border: 4px solid #ff00ff;
}

.encounter {
  display: flex;
  align-self: center;
  margin-bottom: auto;
}

.discard {
  filter: grayscale(1);
}

.focused {
  display: flex;
  flex-wrap: wrap;
}
</style>
