<script lang="ts" setup>

import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import Player from '@/marvel/components/Player.vue'
import Villain from '@/marvel/components/Villain.vue'
import MainScheme from '@/marvel/components/MainScheme.vue'
import SideScheme from '@/marvel/components/SideScheme.vue'
import CardOverlay from '@/marvel/components/CardOverlay.vue';

const props = defineProps<{
  game: Game
  identityId: string
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const topOfDiscard = computed(() => props.game.scenario.scenarioDiscard[0])

const focusedCards = computed(() => props.game.focusedCards)
</script>

<template>
  <div class="scenario">
    <div class="encounter">
      <Card v-if="topOfDiscard" :card="topOfDiscard" :game="game" :identityId="identityId" class="discard" />
      <img src="/img/marvel/encounter-back.png" alt="deck" width="100" height="140" class="deck" />

      <Villain
        v-for="villain in game.villains"
        :key="villain.villainId"
        :villain="villain"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />

      <MainScheme
        v-for="mainScheme in game.mainSchemes"
        :key="mainScheme.mainSchemeId"
        :mainScheme="mainScheme"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />

      <SideScheme
        v-for="sideScheme in game.sideSchemes"
        :key="sideScheme.sideSchemeId"
        :sideScheme="sideScheme"
        :identityId="identityId"
        :game="game"
        @choose="emit('choose', $event)"
      />
    </div>
    <div class="player">
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
    <div class="details">
      <CardOverlay />
    </div>
  </div>
</template>

<style scoped lang="scss">
.scenario {
  display: grid;
  grid-template-areas:
    "encounter details"
    "player    details";
  grid-template-columns: 1fr min(300px, 25vw);
  height: 100%;
  border-radius: 10px;
}

.player {
  grid-area: player;
}

.active {
  border: 4px solid #ff00ff;
}

.encounter {
  display: flex;
  align-self: center;
  margin-bottom: auto;
  grid-area: encounter;
}

.discard {
  filter: grayscale(1);
}

.focused {
  display: flex;
  flex-wrap: wrap;
}

.details {
  position: relative;
  width: 150px;
  grid-area: details;
}
</style>
