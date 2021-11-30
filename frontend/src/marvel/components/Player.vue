<template>
  <div class="table">
    <Ally
      v-for="ally in allies"
      :key="ally.contents.allyId"
      :ally="ally"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
    <Support
      v-for="support in supports"
      :key="support.contents.supportId"
      :support="support"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
    <Upgrade
      v-for="upgrade in upgrades"
      :key="upgrade.contents.upgradeId"
      :upgrade="upgrade"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
    <Minion
      v-for="minion in minions"
      :key="minion.contents.minionId"
      :minion="minion"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
  </div>
  <div class="identity">
    <Card v-if="topOfDiscard" :card="topOfDiscard" :game="game" :identityId="identityId" class="discard" />
    <div>
      <div class="identityCard">
        <img :src="playerImg" alt="player" width="150" class="identityCardImg" :class="{ exhausted: player.exhausted, active: activeAbility !== -1 }" @click="emit('choose', activeAbility)" />
        <div class="info">
          <div>HP: {{player.currentHP}}</div>
          <div v-if="player.stunned">stunned</div>
          <div v-if="player.confused">confused</div>
          <div v-if="player.tough">tough</div>
          <div v-if="player.encounterCards.length > 0">Encounter cards: {{player.encounterCards.length}}</div>
          <AbilityButton
                v-for="ability in abilities"
                :key="ability"
                :ability="choices[ability]"
                :data-image="image"
                @click="emit('choose', ability)"
                />
          <button
            v-if="defendAction !== -1"
            @click="emit('choose', defendAction)"
          >Defend</button>
          <button
            v-for="label in labels"
            :key="label"
            @click="emit('choose', label)"
            >{{choices[label].contents}}</button>
          <button
            v-if="finishPaymentAction !== -1"
            @click="emit('choose', finishPaymentAction)"
          >Finish Payment</button>
          <button
            :disabled="endTurnAction === -1"
            @click="emit('choose', endTurnAction)"
          >End turn</button>
          <button @click="undo">Undo</button>
          <button @click="toggleDebug">Toggle Debug</button>
        </div>
      </div>
    </div>
    <div>
      <img src="/img/marvel/player-back.png" alt="deck" width="150" height="209" class="deck" />
      <template v-if="debug">
        <button @click="debugChoose({tag: 'IdentityMessage', contents: [identityId, {tag: 'SearchIdentityDeck', contents: [{tag: 'AnyCard', contents: []}, {tag: 'SearchDrawOne', contents: []}]}]})">Select Draw</button>
      </template>
    </div>
    <Card v-for="(card, idx) in player.hand" :key="idx" :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" />
  </div>
</template>

<script lang="ts" setup>

import { defineProps, defineEmits, computed, inject } from 'vue'
import { Game } from '@/marvel/types/Game'
import * as MarvelGame from '@/marvel/types/Game'
import { Identity } from '@/marvel/types/Identity'
import Card from '@/marvel/components/Card.vue'
import Ally from '@/marvel/components/Ally.vue'
import Minion from '@/marvel/components/Minion.vue'
import Support from '@/marvel/components/Support.vue'
import Upgrade from '@/marvel/components/Upgrade.vue'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  player: Identity
  identityId: string
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

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
const supports = computed(() => props.player.supports.map((supportId) => props.game.supports[supportId]))
const upgrades = computed(() => props.player.upgrades.map((upgradeId) => props.game.upgrades[upgradeId]))
const minions = computed(() => props.player.minions.map((minionId) => props.game.minions[minionId]))

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

const activeAbility = computed(() => {
  return choices.value.findIndex((choice) => {
    if (choice.tag !== 'TargetLabel') {
      return false
    }

    const { contents } = choice.target
      if (typeof contents === "string") {
        return contents == props.player.id
      }

      switch (contents.tag) {
        case 'IdentityCharacter':
          return contents.contents === props.player.id
        default:
          return false
      }
  })
})

const debug = inject('debug')
const debugChoose = inject('debugChoose')
const undo = inject('undo')
const toggleDebug = inject('toggleDebug')
</script>

<style scoped lang="scss">
.identity {
  display: flex;
  padding: 10px;
}

.discard {
  filter: grayscale(1);
}

.deck {
  margin-left: 2px;
  border-radius: 10px;
}

.exhausted {
  transform: rotate(90deg);
  display: block;
  margin: 0 30px;
}

.active {
  border: 2px solid #FF00FF;
}

.identityCard {
  display: flex;
  flex-flow: row;
  .info {
    display: flex;
    flex-flow: column;
    margin: 0 10px;
  }
}

.table {
  padding: 10px;
}

</style>
