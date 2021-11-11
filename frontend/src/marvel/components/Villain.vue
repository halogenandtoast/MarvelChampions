<template>
  <div class="villain">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)"/>
    <div class="hp">{{villain.contents.villainHp}}</div>
    <div v-if="villain.contents.villainStunned">Stunned</div>
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
    const card = computed(() => ({ cardId: props.villain.contents.villainId, cardDef: props.villain.contents.villainCardDef }))

    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.value.findIndex((choice) => choice.tag === 'TargetLabel' && choice.target.contents == props.villain.contents.villainId)
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
  font-size: 1.2em;
  background: #8E1C21;
  display: inline-block;
  padding: 0 10px;
  border: 5px solid #383839;
  color: white;
  border-radius: 10px;
}
</style>
