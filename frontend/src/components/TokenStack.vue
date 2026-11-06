<script setup lang="ts">
import { assetUrl } from '../assets'
// A count made of the printed tokens, as it would be on the table: the largest first.
import { computed } from 'vue'

const { kind, count } = defineProps<{ kind: 'damage' | 'threat' | 'generic' | 'acceleration'; count: number }>()
const values: Record<typeof kind, number[]> = { damage: [5, 3, 1], threat: [5, 3, 1], generic: [3, 1], acceleration: [1] }
const tokens = computed(() => {
  const out: string[] = []
  let left = count
  for (const v of values[kind]) for (; left >= v; left -= v) out.push(kind === 'acceleration' ? 'acceleration' : `${kind}-${v}`)
  return out
})
</script>

<template>
  <span class="tokens">
    <img v-for="(token, i) in tokens" :key="i" :src="assetUrl(`/images/tokens/${token}.avif`)" alt="" draggable="false" />
  </span>
</template>

<style scoped>
.tokens { display: inline-flex; }
img { width: var(--token-size, 26px); height: auto; filter: drop-shadow(0 2px 3px rgb(0 0 0 / 0.6)); }
img + img { margin-left: calc(var(--token-size, 26px) * -0.45); }
</style>
