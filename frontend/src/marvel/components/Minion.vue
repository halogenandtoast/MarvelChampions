<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Minion } from '@/marvel/types/Minion'
import Upgrade from '@/marvel/components/Upgrade.vue'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  minion: Minion
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.minion.contents.minionId, cardDef: props.minion.contents.minionCardDef }))
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
          return contents == props.minion.contents.minionId
        }

        switch (contents.tag) {
          case 'EnemyMinionId':
            return contents.contents === props.minion.contents.minionId
          default:
            return false
        }

    })
})

const upgrades = computed(() => props.minion.contents.minionUpgrades.map((upgradeId) => props.game.upgrades[upgradeId]))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.minion.contents.minionId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="minion">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="emit('choose', activeAbility)" />
    <div v-if="minion.contents.minionDamage > 0" class="damage">{{minion.contents.minionDamage}}</div>
    <div v-if="minion.contents.minionTough" class="tough">tough</div>
    <div v-if="minion.contents.minionConfused" class="confused">confused</div>
    <div v-if="minion.contents.minionStunned" class="stunned">stunned</div>
    <Upgrade
      v-for="upgrade in upgrades"
      :key="upgrade.contents.upgradeId"
      :upgrade="upgrade"
      :game="game"
      :identityId="identityId"
      class="attached"
      @choose="emit('choose', $event)"
    />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          @click="emit('choose', ability)"
          />
  </div>
</template>

<style scoped lang="scss">
.minion {
  display: inline-block;
}

.attached {
  ::v-deep(img) {
    object-fit: cover;
    object-position: 0% bottom;
    height: 90px;
  }
}
</style>
