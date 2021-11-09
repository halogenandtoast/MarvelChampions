<template>
  <div class="villain">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" />
    <div>{{villain.contents.villainHp}} / {{villain.contents.villainMaxHp}}</div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { Villain } from '@/marvel/types/Villain'
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

    return { card }
  }
})
</script>

<style scoped lang="scss">
.villain {
  display: inline-block;
}
</style>
