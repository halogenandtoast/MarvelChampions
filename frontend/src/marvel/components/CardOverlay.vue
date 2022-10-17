<script lang="ts" setup>
import { ref } from 'vue';
const card = ref<string | null>(null);
document.addEventListener('mouseover', (event) => {
  if (event.target instanceof HTMLImageElement) {
    if (event.target.classList.contains('card') || event.target.dataset.role == "card") {
      card.value = event.target.src
    }
  } else if (event.target instanceof HTMLDivElement) {
    if (event.target.classList.contains('card')) {
      card.value = event.target.style.backgroundImage.slice(4, -1).replace(/"/g, "")
    }
  } else if (event.target instanceof HTMLElement) {
    if(event.target.dataset.imageId) {
      card.value = `/img/marvel/cards/${event.target.dataset.imageId}.jpg`
    }
    if(event.target.dataset.image) {
      card.value = event.target.dataset.image
    }
  }
})
</script>

<template>
  <div class="card-overlay">
    <img v-if="card" :src="card" />
  </div>
</template>

<style lang="scss">
.card-overlay {
  width: min(300px, 25vw);
  img { width: 100%; }
}

</style>
