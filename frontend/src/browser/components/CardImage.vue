<script setup lang="ts">
// A card's scan in a box of the right shape for it, reserved before it loads so a
// scrolling grid never reflows.
import CardFace from '../../components/CardFace.vue'
import type { Card } from '@browser/types/card'

const { card, eager = false, tight = false } = defineProps<{
  card: Card
  eager?: boolean
  /** crop for a very small box, where the middle of a card shows nothing */
  tight?: boolean
}>()
</script>

<template>
  <div class="frame" :class="{ 'frame--tight': tight }" :data-orientation="card.orientation ?? 'portrait'" :style="{ aspectRatio: card.aspectRatio }">
    <CardFace :code="card.code" :name="card.name" :eager="eager" />
  </div>
</template>

<style scoped>
.frame { position: relative; overflow: hidden; border-radius: var(--radius-card, 6px); background: var(--ink-4); aspect-ratio: 63 / 88; box-shadow: 0 0 0 1px rgb(255 255 255 / 0.06); }
.frame[data-orientation='landscape'] { aspect-ratio: 88 / 63; }
.frame--tight :deep(img) { object-position: 50% 14%; }
</style>
