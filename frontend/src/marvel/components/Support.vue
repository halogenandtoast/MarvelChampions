<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Support } from '@/marvel/types/Support'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  support: Support
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.support.supportId, cardDef: props.support.supportCardDef }))
const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.support.supportId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="support">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ exhausted: support.supportExhausted }" />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          :data-image="image"
          @click="emit('choose', ability)"
          />
    <div v-if="support.supportUses > 0">Uses: {{support.supportUses}}</div>
  </div>
</template>

<style scoped lang="scss">
.support {
  display: inline-block;
}

.exhausted {
  transform: rotate(90deg);
  margin: 0 30px;
}
</style>
