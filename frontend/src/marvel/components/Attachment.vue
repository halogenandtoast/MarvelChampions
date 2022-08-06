<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Attachment } from '@/marvel/types/Attachment'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  attachment: Attachment
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.attachment.attachmentId, cardDef: props.attachment.attachmentCardDef }))
const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const activeAbility = computed(() => {
  return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.attachment.attachmentId)
})

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.attachment.attachmentId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="attachment">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ active: activeAbility !== -1 }" />
    <div v-if="attachment.attachmentDamage > 0" class="damage">damage: {{attachment.attachmentDamage}}</div>
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          :data-image="image"
          @click="emit('choose', ability)"
          />
  </div>
</template>

<style scoped lang="scss">
.attachment {
  display: inline-block;
}
</style>
