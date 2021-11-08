<template>
  <button>{{label}}</button>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { UseAbility } from '@/marvel/types/Question';
export default defineComponent({
  props: {
    ability: { type: Object as () => UseAbility, required: true }
  },
  setup(props) {
    const label = computed(() => {
      const { abilityLabel, abilityType } = props.ability.contents
      if (abilityLabel) {
        return abilityLabel
      }

      if (abilityType == "ForcedResponse") {
        return "Forced Response"
      }

      if (abilityType == "Response") {
        return "Response"
      }

      return null
    })
    return { label }
  }
})
</script>
