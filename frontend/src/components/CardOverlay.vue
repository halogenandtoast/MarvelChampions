<script setup lang="ts">
import { assetUrl } from '../assets'
import { useI18n } from 'vue-i18n'
import { computed } from 'vue'
import { STATUS_WIDTH, useCardOverlay } from '../composables/useCardOverlay'
import CardFace from './CardFace.vue'

const { card, sideways, side, placement, visible, teleportTarget } = useCardOverlay()
const { t } = useI18n()
const statusGroups = computed(() => [...new Set(card.value?.statuses ?? [])].map(status => ({ status, count: card.value!.statuses!.filter(value => value === status).length })))
</script>

<template>
  <Teleport :to="teleportTarget">
    <div
      v-if="visible && card"
      class="card-overlay"
      :class="{ sideways }"
      :style="{ top: `${placement.top}px`, left: `${placement.left}px`, width: `${placement.width}px`, height: `${placement.height}px` }"
    >
      <div class="face"><CardFace :code="card.code" :back="card.back ?? null" :name="card.name" eager /></div>
      <!-- its status cards, on the side away from the card hovered; ponytail: not scaled down with a pinned preview on a small screen -->
      <div v-if="card.statuses?.length" class="statuses" :class="side" :style="{ width: `${STATUS_WIDTH}px` }">
        <div v-for="group in statusGroups" :key="group.status" class="status-group" :class="{ multiple: group.count > 1 }">
          <img v-for="n in group.count" :key="n" :src="assetUrl(`/images/tokens/${group.status}.avif`)" :class="{ inactive: card.inactiveStatuses?.includes(group.status) }" :alt="card.inactiveStatuses?.includes(group.status) ? t('game.status.inactive', { status: t(`game.status.${group.status}`) }) : t(`game.status.${group.status}`)" />
        </div>
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
.status-group { display: flex; }
.status-group.multiple img { width: 62.5%; flex: none; }
.status-group.multiple img + img { margin-left: -25%; }
.statuses img.inactive { filter: grayscale(1); }
@keyframes overlay-in {
  from { opacity: 0; transform: scale(0.97); }
  to { opacity: 1; transform: none; }
}
</style>
