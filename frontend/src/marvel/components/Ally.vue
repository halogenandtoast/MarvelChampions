<template>
  <div class="ally">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ exhausted: ally.contents.allyExhausted, active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)" />
    <div v-if="ally.contents.allyDamage > 0" class="damage">damage: {{ally.contents.allyDamage}}</div>
    <div v-if="ally.contents.allyCounters > 0" class="counter">counters: {{ally.contents.allyCounters}}</div>
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
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Ally } from '@/marvel/types/Ally'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

export default defineComponent({
  components: { Card, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    ally: { type: Object as () => Ally, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.ally.contents.allyId, cardDef: props.ally.contents.allyCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const abilities = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.ally.contents.allyId) {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    const defendAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === 'AllyDefend' && c.contents == props.ally.contents.allyId)
    })

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => {
        if (choice.tag !== 'TargetLabel') {
          return false
        }

        const { contents } = choice.target
          if (typeof contents === "string") {
            return contents == props.ally.contents.allyId
          }

          switch (contents.tag) {
            case 'AllyCharacter':
              return contents.contents === props.ally.contents.allyId
            default:
              return false
          }
      })
    })

    return { card, abilities, choices, defendAction, activeAbility }
  }
})
</script>

<style scoped lang="scss">
.ally {
  display: inline-block;
}

.exhausted {
  transform: rotate(90deg);
}
</style>
