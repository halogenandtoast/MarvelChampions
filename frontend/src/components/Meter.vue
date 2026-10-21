<script setup lang="ts">
// A count against a limit, as a bar: a villain's hit points, the threat on a scheme.
import { computed } from 'vue'

const { label, value, max = null, tone = 'health', invert = false } = defineProps<{
  label: string
  value: number
  max?: number | null
  tone?: 'health' | 'threat' | 'villain'
  /** fill by what is gone rather than what is left */
  invert?: boolean
}>()
const share = computed(() => (max ? Math.min(100, Math.max(0, ((invert ? max - value : value) / max) * 100)) : 0))
</script>

<template>
  <div class="meter" :class="`tone-${tone}`">
    <span class="label">{{ label }}</span>
    <span class="value">{{ value }}<small v-if="max !== null">/{{ max }}</small></span>
    <span v-if="max !== null" class="track"><span class="fill" :style="{ width: `${share}%` }" /></span>
  </div>
</template>

<style scoped>
.meter {
  --tone: var(--health);
  display: grid;
  grid-template-columns: auto 1fr;
  align-items: baseline;
  gap: 2px 8px;
  width: 100%;
  min-width: 0;
}
.meter.tone-threat { --tone: var(--amber); }
.meter.tone-villain { --tone: var(--crimson-bright); }
.label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--text-faint); }
.value { justify-self: end; font-family: var(--font-display); font-size: 1.05rem; font-weight: 700; line-height: 1; color: var(--text); font-variant-numeric: tabular-nums; }
.value small { font-size: 0.75em; color: var(--text-faint); }
.track { grid-column: 1 / -1; height: 5px; border-radius: 3px; background: rgb(255 255 255 / 0.08); overflow: hidden; }
.fill { display: block; height: 100%; border-radius: inherit; background: linear-gradient(90deg, color-mix(in srgb, var(--tone) 70%, black), var(--tone)); transition: width 0.4s ease; }
</style>
