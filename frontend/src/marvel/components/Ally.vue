<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Ally } from '@/marvel/types/Ally'
import AbilityButton from '@/marvel/components/AbilityButton.vue'
import Upgrade from '@/marvel/components/Upgrade.vue'

const props = defineProps<{
  game: Game
  identityId: string
  ally: Ally
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.ally.allyId, cardDef: props.ally.allyCardDef }))
const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.ally.allyId) {
      return [...acc, i]
    }
    return acc
  }, [])
})

const defendAction = computed(() => {
  return choices
    .value
    .findIndex((c) => c.tag === 'AllyDefend' && c.contents == props.ally.allyId)
})

const activeAbility = computed(() => {
  return choices.value.findIndex((choice) => {
    if (choice.tag !== 'TargetLabel') {
      return false
    }

    const { contents } = choice.target
      if (typeof contents === "string") {
        return contents == props.ally.allyId
      }

      switch (contents.tag) {
        case 'AllyCharacter':
          return contents.contents === props.ally.allyId
        default:
          return false
      }
  })
})

const upgrades = computed(() => props.ally.allyUpgrades.map((upgradeId) => props.game.upgrades[upgradeId]))
</script>

<template>
  <div class="ally">
    <div class="contents">
      <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ exhausted: ally.allyExhausted, active: activeAbility !== -1 }" @click="emit('choose', activeAbility)" />
      <Upgrade
        v-for="upgrade in upgrades"
        :key="upgrade.upgradeId"
        :upgrade="upgrade"
        :game="game"
        :identityId="identityId"
        class="attached"
        @choose="emit('choose', $event)"
      />
      <div v-if="ally.allyDamage > 0" class="damage">damage: {{ally.allyDamage}}</div>
      <div v-if="ally.allyCounters > 0" class="counter">counters: {{ally.allyCounters}}</div>
      <div v-if="ally.allyStunned">Stunned</div>
      <div v-if="ally.allyConfused">Confused</div>
      <div v-if="ally.allyTough">Tough</div>
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
    </div>
  </div>
</template>

<style scoped lang="scss">
.ally {
  display: inline-block;
  .contents {
    display: flex;
    flex-direction: column;
  }
}

.exhausted {
  transform: rotate(90deg);
  margin: 0 30px;
}

.attached {
  ::v-deep(img) {
    object-fit: cover;
    object-position: 0% bottom;
    height: 90px;
  }
}
</style>
