<script setup lang="ts">
import { computed } from 'vue'
import GameIcon from '../browser/components/GameIcon.vue'

const { text } = defineProps<{ text: string }>()
// Server choices use brace tokens. Keep everything else as plain text, including
// unknown tokens and markup; only recognized resource symbols become components.
const resource = /^\{(physical|mental|energy|wild)\}$/
const parts = computed(() => text.split(/(\{(?:physical|mental|energy|wild)\})/g)
  .map(value => ({ value, icon: resource.exec(value)?.[1] })))
</script>

<template>
  <template v-for="(part, i) in parts" :key="i">
    <GameIcon v-if="part.icon" :name="part.icon" :label="$t(`game.resources.${part.icon}`)" />
    <template v-else>{{ part.value }}</template>
  </template>
</template>
