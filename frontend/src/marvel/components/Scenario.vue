<template>
  <div class="scenario">
    <div class="encounter">
      <Villain
        v-for="villain in game.villains"
        :key="villain.contents.villainId"
        :villain="villain"
        :identityId="identityId"
        :game="game"
        @choose="$emit('choose', $event)"
      />
      <Card v-if="topOfDiscard" :card="topOfDiscard" :game="game" :identityId="identityId" class="discard" />
      <img src="/img/marvel/encounter-back.png" alt="deck" width="150" height="209" class="deck" />
      <div class="mainScheme">
        <img :src="scenarioImg" alt="Scenario" height="200" class="scenario-card card" :class="{ active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)" />
        <div>{{game.scenario.contents.scenarioThreat}}</div>
        <div v-if="game.scenario.contents.scenarioAccelerationTokens > 0">Acceleration Tokens: {{game.scenario.contents.scenarioAccelerationTokens}}</div>
      </div>

      <SideScheme
        v-for="sideScheme in game.sideSchemes"
        :key="sideScheme.contents.sideSchemeId"
        :sideScheme="sideScheme"
        :identityId="identityId"
        :game="game"
        @choose="$emit('choose', $event)"
      />
    </div>
    <div v-if="focusedCards.length > 0" class="focused">
      <Card v-for="card in focusedCards" :key="card.contents.cardId" :card="card.contents" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" />
    </div>
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
import Card from '@/marvel/components/Card.vue'
import Player from '@/marvel/components/Player.vue'
import Villain from '@/marvel/components/Villain.vue'
import SideScheme from '@/marvel/components/SideScheme.vue'
import * as MarvelGame from '@/marvel/types/Game'

export default defineComponent({
  props: { game: { type: Object as () => Game, required: true }, identityId: { type: String, required: true } },
  components: { Player, Villain, SideScheme, Card },
  setup(props) {
    const scenarioImg = computed(() => `/img/marvel/cards/${props.game.scenario.contents.scenarioId}.jpg`)

    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.
        value.
        findIndex((choice) => {
          if (choice.tag !== 'TargetLabel') {
            return false
          }

          const { contents } = choice.target
          if (typeof contents === "string") {
            return contents == props.game.scenario.contents.scenarioId
          }

          switch (contents.tag) {
            case 'SchemeMainSchemeId':
              return contents.contents === props.game.scenario.contents.scenarioId
            default:
              return false
          }
        })
    })

    const topOfDiscard = computed(() => props.game.scenario.contents.scenarioDiscard[0])

    const focusedCards = computed(() => props.game.focusedCards)

    return { scenarioImg, activeAbility, topOfDiscard, focusedCards }
  }
})
</script>

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
}

.discard {
  filter: grayscale(1);
}

.focused {
  display: flex;
  flex-wrap: wrap;
}
</style>
