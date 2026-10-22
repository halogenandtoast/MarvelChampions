<script setup lang="ts">
// Stunned, confused and tough: their status cards, tucked under the corner of the card. A
// hover shows the status card full size; leaving it hands the preview back to the card.
import { useI18n } from 'vue-i18n'
import { useCardOverlay } from '../composables/useCardOverlay'

const { stunned = false, confused = false, tough = false } = defineProps<{ stunned?: boolean; confused?: boolean; tough?: boolean }>()
const emit = defineEmits<{ leave: [] }>()
const { t } = useI18n()
const overlay = useCardOverlay()
const shown = () => (['stunned', 'confused', 'tough'] as const).filter((s) => ({ stunned, confused, tough })[s])
const enter = (status: string, event: PointerEvent) => {
  if (event.pointerType !== 'mouse') return
  overlay.show({ code: null, image: `/images/tokens/${status}.avif`, name: t(`game.status.${status}`) }, event.currentTarget as HTMLElement, true)
}
const leave = (event: PointerEvent) => {
  if (event.pointerType === 'mouse') emit('leave')
}
</script>

<template>
  <span v-if="stunned || confused || tough" class="statuses">
    <img
      v-for="status in shown()"
      :key="status"
      :src="`/images/tokens/${status}.avif`"
      :alt="t(`game.status.${status}`)"
      :title="t(`game.status.${status}`)"
      draggable="false"
      @pointerenter="enter(status, $event)"
      @pointerleave="leave"
    />
  </span>
</template>

<style scoped>
.statuses { display: flex; flex-direction: column; align-items: flex-start; z-index: 1; }
img { width: max(40px, 52cqi); height: auto; border-radius: 4px; box-shadow: 0 2px 5px rgb(0 0 0 / 0.55); }
img + img { margin-top: max(-18px, -22cqi); }
</style>
