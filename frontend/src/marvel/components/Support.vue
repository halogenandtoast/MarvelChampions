<template>
  <div class="support">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ exhausted: support.contents.supportExhausted }" />
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
import { Support } from '@/marvel/types/Support'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

export default defineComponent({
  components: { Card, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    support: { type: Object as () => Support, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.support.contents.supportId, cardDef: props.support.contents.supportCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const abilities = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.support.contents.supportId) {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    return { card, abilities, choices }
  }
})
</script>

<style scoped lang="scss">
.support {
  display: inline-block;
}

.exhausted {
  transform: rotate(90deg);
}
</style>
