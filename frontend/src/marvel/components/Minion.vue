<template>
  <div class="minion">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)" />
    <div v-if="minion.contents.minionDamage > 0" class="damage">{{minion.contents.minionDamage}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Minion } from '@/marvel/types/Minion'

export default defineComponent({
  components: { Card },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    minion: { type: Object as () => Minion, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.minion.contents.minionId, cardDef: props.minion.contents.minionCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.minion.contents.minionId)
    })

    return { card, activeAbility, choices }
  }
})
</script>

<style scoped lang="scss">
.minion {
  display: inline-block;
}
</style>
