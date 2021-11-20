<template>
  <div class="upgrade">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ exhausted: upgrade.contents.upgradeExhausted, active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)" />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          :data-image="image"
          @click="$emit('choose', ability)"
          />
    <div v-if="upgrade.contents.upgradeUses > 0">{{upgrade.contents.upgradeUses}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Upgrade } from '@/marvel/types/Upgrade'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

export default defineComponent({
  components: { Card, AbilityButton },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    upgrade: { type: Object as () => Upgrade, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.upgrade.contents.upgradeId, cardDef: props.upgrade.contents.upgradeCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const abilities = computed(() => {
      return choices.value.reduce<number[]>((acc, v, i) => {
        if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.upgrade.contents.upgradeId) {
          return [...acc, i]
        }
        return acc
      }, [])
    })

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.upgrade.contents.upgradeId)
    })

    return { card, abilities, choices, activeAbility }
  }
})
</script>

<style scoped lang="scss">
.upgrade {
  display: inline-block;
}

.active {
  border: 1px solid #FF00FF;
}

.exhausted {
  transform: rotate(90deg);
}
</style>