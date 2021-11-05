<template>
  <div class="card">
    <img :src="cardImage" alt="card" />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { CardDef } from '@/marvel/types/CardDef'

export default defineComponent({
  props: { card: { type: Object as () => CardDef, required: true } },
  setup(props) {
    const cardImage = computed(() => `/img/marvel/cards/${props.card.cdCardCode}.jpg`)

    return { cardImage }
  }
})
</script>

<style scoped lang="scss">
.card {
  img {
    width: 150px;
    margin: 2px;
  }
}
</style>
