<script setup lang="ts">
import { useCardOverlay } from '../composables/useCardOverlay'
import CardFace from './CardFace.vue'

const { card, sideways, placement, visible } = useCardOverlay()
</script>

<template>
  <Teleport to="body">
    <div
      v-if="visible && card"
      class="card-overlay"
      :class="{ sideways }"
      :style="{ top: `${placement.top}px`, left: `${placement.left}px`, width: `${placement.width}px`, height: `${placement.height}px` }"
    >
      <img v-if="card.image" class="image" :src="card.image" :alt="card.name ?? ''" />
      <CardFace v-else :code="card.code" :back="card.back ?? null" :name="card.name" eager />
    </div>
  </Teleport>
</template>

<style scoped>
.card-overlay {
  position: fixed;
  z-index: 9999;
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 18px 50px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12);
  pointer-events: none;
  animation: overlay-in 0.12s ease-out;
  background: var(--ink-3);
}
.image { display: block; width: 100%; height: 100%; object-fit: cover; }
@keyframes overlay-in {
  from { opacity: 0; transform: scale(0.97); }
  to { opacity: 1; transform: none; }
}
</style>
