<template>
  <div class="villain">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)"/>
    <div class="hp">{{villain.contents.villainHp}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { Villain } from '@/marvel/types/Villain'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'

export default defineComponent({
  components: { Card },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    villain: { type: Object as () => Villain, required: true }
  },
  setup(props) {
    const card = computed(() => ({ pcCardId: props.villain.contents.villainId, pcCardDef: props.villain.contents.villainCardDef }))

    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel')
    })

    return { card, activeAbility }
  }
})
</script>

<style scoped lang="scss">
.villain {
  display: inline-block;
}

.hp {
  font-family: "Exo2";
  font-weight: bold;
  background: #8E1C21;
  display: inline-block;
  padding: 0 5px;
  border: 10px solid #383839;
  color: white;
  border-radius: 10px;
}
</style>
