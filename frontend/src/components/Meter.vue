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
  <div class="meter" :class="`tone-${tone}`" role="meter" :aria-label="label" :aria-valuenow="value" :aria-valuemax="max ?? undefined" :title="label">
    <span v-if="max !== null" class="fill" :style="{ width: `${share}%` }" />
    <span class="value">{{ value }}<small v-if="max !== null">/{{ max }}</small></span>
  </div>
</template>

<style scoped>
/* the count sits inside its bar */
.meter {
  --tone: var(--health);
  position: relative;
  display: grid;
  place-items: center;
  width: 100%;
  min-width: 0;
  height: 20px;
  border-radius: 5px;
  background: rgb(255 255 255 / 0.08);
  overflow: hidden;
}
.meter.tone-threat { --tone: var(--amber); }
.meter.tone-villain { --tone: var(--crimson-bright); }
.fill { position: absolute; inset: 0 auto 0 0; background: linear-gradient(90deg, color-mix(in srgb, var(--tone) 55%, black), color-mix(in srgb, var(--tone) 85%, black)); transition: width 0.4s ease; }
.value { position: relative; font-family: var(--font-display); font-size: 0.95rem; font-weight: 700; line-height: 1; color: #fff; text-shadow: 0 1px 2px rgb(0 0 0 / 0.7); font-variant-numeric: tabular-nums; }
.value small { font-size: 0.8em; color: rgb(255 255 255 / 0.75); }
</style>
