<template>
  <div class="player">
    <div class="table">
      <Ally v-for="ally in allies" :key="ally.contents.allyId" :ally="ally" :game="game" :player="player" @choose="$emit('choose', $event)" />
    </div>
    <div class="identity">
      <img :src="playerImg" alt="player" width="150" />
      <Card v-for="(card, idx) in player.hand" :key="idx" :card="card" :game="game" :player="player" @choose="$emit('choose', $event)" />
    </div>
    <button
      v-if="finishPaymentAction !== -1"
      @click="$emit('choose', finishPaymentAction)"
    >Finish Payment</button>
    <button
      :disabled="changeFormAction === -1"
      @click="$emit('choose', changeFormAction)"
    >Change Form</button>
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

export default defineComponent({
  components: { Card, Ally },
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

    const playerImg = computed(() => `/img/marvel/cards/${playerCardCode.value}.jpg`)

    const choices = computed(() => MarvelGame.choices(props.game, props.player.id))

    const endTurnAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'EndTurn')
    })

    const changeFormAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'UseAbility' && c.contents.abilityChoice.tag === 'ChangeForm')
    })

    const finishPaymentAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'FinishPayment')
    })

    const allies = computed(() => props.player.allies.map((allyId) => props.game.allies[allyId]))

    return { playerImg, choices, endTurnAction, changeFormAction, finishPaymentAction, allies }
  }
})
</script>

<style scoped lang="scss">
.identity {
  display: flex;
}
</style>
