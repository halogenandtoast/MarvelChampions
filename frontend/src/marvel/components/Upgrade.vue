<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Upgrade } from '@/marvel/types/Upgrade'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  upgrade: Upgrade
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.upgrade.upgradeId, cardDef: props.upgrade.upgradeCardDef }))
const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.upgrade.upgradeId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="upgrade">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ exhausted: upgrade.upgradeExhausted }" />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          :data-image="image"
          @click="emit('choose', ability)"
          />
    <div v-if="upgrade.upgradeUses > 0">{{upgrade.upgradeUses}}</div>
  </div>
</template>

<style scoped lang="scss">
.upgrade {
  display: inline-block;
}

.active {
  border: 1px solid #FF00FF;
}

.exhausted {
  transform: rotate(90deg);
  margin: 0 30px;
}
</style>
