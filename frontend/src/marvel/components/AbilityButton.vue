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
      const { abilityLabel, abilityType, abilityChoices } = props.ability.contents
      if (abilityLabel) {
        return abilityLabel
      }

      if (abilityType == "Resource") {
        return "Resource"
      }

      if (abilityType == "ForcedResponse") {
        return "Forced Response"
      }

      if (abilityType == "ForcedInterrupt") {
        return "Forced Interrupt"
      }

      if (abilityType == "Response") {
        return "Response"
      }

      if (abilityType == "Basic") {
        const basicType = abilityChoices[0]?.tag
        switch(basicType) {
          case 'AllyAttack': return 'Attack'
          case 'AllyThwart': return 'Thwart'
          default: return basicType
        }
      }

      if (abilityType == "Action") {
        const actionTag = abilityChoices[0]?.tag
        if (actionTag == "ChangeForm") {
          return "Change Form"
        }

        return "Action"
      }

      if (abilityType == "AlterEgoAction") {
        return "Alter-Ego Action"
      }

      if (abilityType == "HeroAction") {
        return "Hero Action"
      }

      return null
    })
    return { label }
  }
})
</script>
