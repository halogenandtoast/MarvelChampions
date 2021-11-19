<template>
  <div class="sideScheme">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" />
    <div v-if="sideScheme.contents.sideSchemeDamage > 0" class="damage">{{sideScheme.contents.sideSchemeDamage}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { SideScheme } from '@/marvel/types/SideScheme'

export default defineComponent({
  components: { Card },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    sideScheme: { type: Object as () => SideScheme, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.sideScheme.contents.sideSchemeId, cardDef: props.sideScheme.contents.sideSchemeCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.sideScheme.contents.sideSchemeId)
    })

    return { card, activeAbility, choices }
  }
})
</script>

<style scoped lang="scss">
.sideScheme {
  display: inline-block;
}
</style>
