<script setup lang="ts">
import { assetUrl } from '../assets'
// A count made of the printed tokens, as it would be on the table: the largest first.
import { computed } from 'vue'

const { kind, count, pulse = 0 } = defineProps<{ kind: 'damage' | 'threat' | 'generic' | 'acceleration'; count: number; pulse?: number }>()
const values: Record<typeof kind, number[]> = { damage: [50, 30, 10, 5, 3, 1], threat: [50, 30, 10, 5, 3, 1], generic: [50, 30, 10, 3, 1], acceleration: [1] }
const tokens = computed(() => {
  const out: string[] = []
  let left = count
  for (const v of values[kind]) for (; left >= v; left -= v) out.push(kind === 'acceleration' ? 'acceleration' : `${kind}-${v}`)
  return out
})
const tokenScale = (token: string) => {
  const value = Number(token.split('-')[1] ?? 1)
  return value >= 50 ? 1.3 : value >= 30 ? 1.24 : value >= 10 ? 1.18 : value >= 5 ? 1.12 : value >= 3 ? 1.06 : 1
}
</script>

<template>
  <span class="tokens" :class="{ pulsing: pulse, 'pool-damage': kind === 'damage', 'pool-threat': kind === 'threat' }">
    <img v-for="(token, i) in tokens" :key="`${pulse}-${i}`" :src="assetUrl(`/images/tokens/${token}.avif`)" :style="{ '--token-scale': tokenScale(token) }" alt="" draggable="false" />
    <span v-if="pulse && kind !== 'threat'" :key="pulse" class="pool-pulse" aria-hidden="true" />
  </span>
</template>

<style scoped>
.tokens { display: inline-flex; align-items: center; }
.tokens.pulsing:not(.token) { position: relative; }
.pool-pulse { position: absolute; inset: -5px; border: 2px solid var(--pool-glow, var(--amber)); border-radius: 999px; pointer-events: none; animation: pool-added 850ms ease-out both; }
.pool-damage { --pool-glow: var(--crimson-bright); }
.pool-threat.pulsing { pointer-events: none; }
.pool-threat.pulsing img { animation: threat-throb 850ms ease-out both; }
/* The image's alpha follows the printed triangular token, including its corners. */
@keyframes threat-throb { 0%, 100% { transform: scale(1); filter: drop-shadow(0 2px 3px rgb(0 0 0 / 0.6)); } 25%, 40% { transform: scale(1.08); filter: drop-shadow(0 0 3px var(--amber)) drop-shadow(0 0 8px var(--amber)); } }
@keyframes pool-added { 0% { opacity: 0; transform: scale(0.92); } 18%, 38% { opacity: 1; transform: scale(1); box-shadow: 0 0 16px var(--pool-glow, var(--amber)); } 100% { opacity: 0; transform: scale(1.15); } }
@media (prefers-reduced-motion: reduce) { .pool-pulse, .pool-threat.pulsing img { animation: none; } }
img { width: calc(var(--token-size, 26px) * var(--token-scale, 1)); height: auto; filter: drop-shadow(0 2px 3px rgb(0 0 0 / 0.6)); }
img + img { margin-left: calc(var(--token-size, 26px) * -0.45); }
</style>
