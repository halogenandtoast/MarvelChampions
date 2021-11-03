<template>
  <div>
    Scenario
  </div>
</template>

<script lang="ts">

import { defineComponent, computed } from 'vue'
import { Game } from '@/marvel/types/Game'

export default defineComponent({
  props: { game: { type: () => Object as Game, require: true } },
  setup(props) {
    const scenarioImg = computed(() => props.game.gameScenario.contents.scenarioVillains[0])
    return { scenarioImg }
  }
})
</script>
