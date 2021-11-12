<template>
  <div class="player">
    <div class="table">
      <Ally
        v-for="ally in allies"
        :key="ally.contents.allyId"
        :ally="ally"
        :game="game"
        :identityId="identityId"
        @choose="$emit('choose', $event)"
      />
    </div>
    <div class="identity">
      <Card v-if="topOfDiscard" :card="topOfDiscard" :game="game" :identityId="identityId" class="discard" />
      <div>
        <div class="identityCard" :class="{ exhausted: player.exhausted }">
          <img :src="playerImg" alt="player" width="150" class="identityCardImg" />
        </div>
        <div>HP: {{player.currentHP}}</div>
        <AbilityButton
              v-for="ability in abilities"
              :key="ability"
              :ability="choices[ability]"
              :data-image="image"
              @click="$emit('choose', ability)"
              />
        <button
          v-if="defendAction !== -1"
          @click="$emit('choose', defendAction)"
        >Defend</button>
        <button
          v-for="label in labels"
          :key="label"
          @click="$emit('choose', label)"
          >{{choices[label].contents}}</button>
      </div>
      <img src="/img/marvel/player-back.png" alt="deck" width="150" height="209" class="deck" />
      <Card v-for="(card, idx) in player.hand" :key="idx" :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" />
    </div>
    <button
      v-if="finishPaymentAction !== -1"
      @click="$emit('choose', finishPaymentAction)"
    >Finish Payment</button>
    <button
      :disabled="endTurnAction === -1"
      @click="$emit('choose', endTurnAction)"
    >End turn</button>
  </div>
</template>

<script lang="ts">

import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import * as MarvelGame from '@/marvel/types/Game'
import { Identity } from '@/marvel/types/Identity'
import Card from '@/marvel/components/Card.vue'
import Ally from '@/marvel/components/Ally.vue'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

export default defineComponent({
  components: { Card, Ally, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    player: { type: Object as () => Identity, required: true },
    identityId: { type: String, required: true }
  },
  setup(props) {
    const playerCardCode = computed(() => {
      const side = props.player.sides[props.player.side]
      switch(side.tag) {
        case 'AlterEgoSide':
          return side.contents.contents.alterEgoCardDef.cdCardCode
        case 'HeroSide':
          return side.contents.contents.heroCardDef.cdCardCode
        default:
          return null
      }
    })

    const topOfDiscard = computed(() => props.player.discard[0])

    const playerImg = computed(() => `/img/marvel/cards/${playerCardCode.value}.jpg`)

    const choices = computed(() => MarvelGame.choices(props.game, props.player.id))

    const endTurnAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'EndTurn')
    })

    const defendAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'Defend')
    })

    const finishPaymentAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'FinishPayment')
    })

    const allies = computed(() => props.player.allies.map((allyId) => props.game.allies[allyId]))

    const abilities = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.player.id) {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    const labels = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'Label') {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    return {
      playerImg,
      choices,
      endTurnAction,
      finishPaymentAction,
      allies,
      abilities,
      labels,
      defendAction,
      topOfDiscard
    }
  }
})
</script>

<style scoped lang="scss">
.identity {
  display: flex;
}

.discard {
  filter: grayscale(1);
}

.deck {
  margin-left: 2px;
  border-radius: 10px;
}

.exhausted {
  margin: auto 30px;
  .identityCardImg {
    transform: rotate(90deg);
    display: block;
  }
}

</style>
