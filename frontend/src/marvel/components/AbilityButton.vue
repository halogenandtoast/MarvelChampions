<template>
  <button>{{label}}</button>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { UseAbility } from '@/arkham/types/Question';
export default defineComponent({
  props: {
    ability: { type: Object as () => UseAbility, required: true }
  },
  setup(props) {
    const label = computed(() => props.ability.contents.abilityLabel)
    return { label }
  }
})
</script>
