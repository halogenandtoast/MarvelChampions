<script setup lang="ts">
import { useI18n } from 'vue-i18n'
import { STATUS_WIDTH, useCardOverlay } from '../composables/useCardOverlay'
import CardFace from './CardFace.vue'

const { card, sideways, side, placement, visible } = useCardOverlay()
const { t } = useI18n()
</script>

<template>
  <Teleport to="body">
    <div
      v-if="visible && card"
      class="card-overlay"
      :class="{ sideways }"
      :style="{ top: `${placement.top}px`, left: `${placement.left}px`, width: `${placement.width}px`, height: `${placement.height}px` }"
    >
      <div class="face"><CardFace :code="card.code" :back="card.back ?? null" :name="card.name" eager /></div>
      <!-- its status cards, on the side away from the card hovered; ponytail: not scaled down with a pinned preview on a small screen -->
      <div v-if="card.statuses?.length" class="statuses" :class="side" :style="{ width: `${STATUS_WIDTH}px` }">
        <img v-for="status in card.statuses" :key="status" :src="`/images/tokens/${status}.avif`" :alt="t(`game.status.${status}`)" />
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.card-overlay {
  position: fixed;
  z-index: 9999;
  pointer-events: none;
  animation: overlay-in 0.12s ease-out;
}
.face {
  width: 100%;
  height: 100%;
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 18px 50px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12);
  background: var(--ink-3);
}
.statuses { position: absolute; top: 0; display: flex; flex-direction: column; gap: 10px; }
.statuses.right { left: calc(100% + 10px); }
.statuses.left { right: calc(100% + 10px); }
.statuses img { display: block; width: 100%; height: auto; border-radius: 10px; box-shadow: 0 12px 30px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12); }
@keyframes overlay-in {
  from { opacity: 0; transform: scale(0.97); }
  to { opacity: 1; transform: none; }
}
</style>
