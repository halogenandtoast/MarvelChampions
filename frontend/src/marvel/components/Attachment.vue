<template>
  <div class="attachment">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" />
    <div v-if="attachment.contents.attachmentDamage > 0" class="damage">damage: {{attachment.contents.attachmentDamage}}</div>
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          :data-image="image"
          @click="$emit('choose', ability)"
          />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Attachment } from '@/marvel/types/Attachment'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

export default defineComponent({
  components: { Card, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    attachment: { type: Object as () => Attachment, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.attachment.contents.attachmentId, cardDef: props.attachment.contents.attachmentCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.attachment.contents.attachmentId)
    })

    const abilities = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.attachment.contents.attachmentId) {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    return { card, abilities, activeAbility, choices }
  }
})
</script>

<style scoped lang="scss">
.attachment {
  display: inline-block;
}
</style>
