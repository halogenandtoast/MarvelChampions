<template>
  <div class="ally">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Ally } from '@/marvel/types/Ally'

export default defineComponent({
  components: { Card },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    ally: { type: Object as () => Ally, required: true },
  },
  setup(props) {
    const card = computed(() => ({ pcCardId: props.ally.contents.allyId, pcCardDef: props.ally.contents.allyCardDef }))
    return { card }
  }
})
</script>
