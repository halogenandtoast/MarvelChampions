<script setup lang="ts">
import { assetUrl } from '../assets'
import { computed } from 'vue'
import { statusCards, inactiveStatusCards, type StatusCardCounts } from '../statusCards'
// Stunned, confused and tough: their status cards, tucked under the corner of the card.
const props = defineProps<StatusCardCounts>()
const statuses = computed(() => statusCards(props))
const groups = computed(() => [...new Set(statuses.value)].map(status => ({ status, count: statuses.value.filter(card => card === status).length })))
const inactive = computed(() => inactiveStatusCards(props))
</script>

<template>
  <span v-if="statuses.length" class="statuses">
    <span v-for="group in groups" :key="group.status" class="status-group" :class="{ multiple: group.count > 1 }">
      <img v-for="n in group.count" :key="n" :src="assetUrl(`/images/tokens/${group.status}.avif`)" :class="{ inactive: inactive.includes(group.status) }" :alt="inactive.includes(group.status) ? $t('game.status.inactive', { status: $t(`game.status.${group.status}`) }) : $t(`game.status.${group.status}`)" draggable="false" />
    </span>
  </span>
</template>

<style scoped>
.statuses { display: flex; flex-direction: column; align-items: flex-start; z-index: 1; }
img { width: max(40px, 52cqi); height: auto; border-radius: 4px; box-shadow: 0 2px 5px rgb(0 0 0 / 0.55); }
.inactive { filter: grayscale(1); }
.status-group { display: flex; align-items: flex-start; gap: 3px; }
.status-group + .status-group { margin-top: calc(-1 * max(6px, 10cqi)); }
.status-group.multiple img { width: max(32px, 44cqi); }
.status-group.multiple img + img { margin-left: calc(-1 * max(12px, 18cqi)); }
</style>
