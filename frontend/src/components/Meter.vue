<script setup lang="ts">
// A count against a limit, as a bar: a villain's hit points, the threat on a scheme.
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'

const { label, value, max = null, tone = 'health', invert = false, adding = 0, subtracting = 0, text = null } = defineProps<{
  label: string
  value: number
  /** shown in place of the count and its limit: ∞ for infinite hit points */
  text?: string | null
  max?: number | null
  tone?: 'health' | 'threat' | 'villain'
  /** fill by what is gone rather than what is left */
  invert?: boolean
  /** about to be added, shown dimmer past the fill, red if it reaches the limit */
  adding?: number
  /** assigned damage awaiting confirmation, shown as a loss preview */
  subtracting?: number
}>()
const poolPulse = ref(0)
let pulseTimer: ReturnType<typeof setTimeout> | undefined
const clearPulse = () => { clearTimeout(pulseTimer); poolPulse.value = 0 }
watch(() => value, (current, previous) => {
  if (tone === 'threat' ? current <= previous : current >= previous) return
  clearTimeout(pulseTimer)
  poolPulse.value++
  pulseTimer = setTimeout(() => { poolPulse.value = 0 }, 850)
})
onMounted(() => window.addEventListener('marvel:cancel-animations', clearPulse))
onBeforeUnmount(() => { clearPulse(); window.removeEventListener('marvel:cancel-animations', clearPulse) })
const pct = (n: number) => (max ? Math.min(100, Math.max(0, (n / max) * 100)) : 0)
const remaining = computed(() => Math.max(0, value - subtracting))
const share = computed(() => pct(invert && max ? max - remaining.value : remaining.value))
const assigned = computed(() => pct(value) - pct(remaining.value))
const pending = computed(() => pct(value + adding) - share.value)
const reaches = computed(() => !!adding && max !== null && value + adding >= max)
</script>

<template>
  <div class="meter" :class="[`tone-${tone}`, { reaches }]" role="meter" :aria-label="label" :aria-valuenow="remaining" :aria-valuemax="max ?? undefined" :aria-valuetext="subtracting ? $t('game.assignedDamagePreview', { remaining, damage: subtracting }) : text ?? undefined" :title="subtracting ? $t('game.assignedDamagePreview', { remaining, damage: subtracting }) : label">
    <span v-if="max !== null" class="fill" :style="{ width: `${share}%` }" />
    <span v-if="max !== null && subtracting" class="assigned" :style="{ left: `${pct(remaining)}%`, width: `${assigned}%` }" />
    <span v-if="max !== null && adding" class="pending" :style="{ left: `${share}%`, width: `${pending}%` }" />
    <span v-if="text !== null" class="value">{{ text }}</span>
    <span v-else class="value">{{ remaining }}<b v-if="adding">+{{ adding }}</b><small v-if="max !== null">/{{ max }}</small></span>
    <span v-if="poolPulse" :key="poolPulse" class="meter-pulse" aria-hidden="true" />
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
.meter-pulse { position: absolute; inset: 0; border: 2px solid var(--crimson-bright); border-radius: inherit; pointer-events: none; animation: pool-changed 850ms ease-out both; }
.tone-threat .meter-pulse { border-color: var(--amber); }
@keyframes pool-changed { 0% { opacity: 0; } 20%, 40% { opacity: 1; } 100% { opacity: 0; } }
@media (prefers-reduced-motion: reduce) { .meter-pulse { animation: none; } }
.fill { position: absolute; inset: 0 auto 0 0; background: linear-gradient(90deg, color-mix(in srgb, var(--tone) 55%, black), color-mix(in srgb, var(--tone) 85%, black)); transition: width 0.4s ease; }
.value { position: relative; font-family: var(--font-display); font-size: 0.95rem; font-weight: 700; line-height: 1; color: #fff; text-shadow: 0 1px 2px rgb(0 0 0 / 0.7); font-variant-numeric: tabular-nums; }
.pending { position: absolute; inset: 0 auto 0 auto; background: color-mix(in srgb, var(--tone) 35%, transparent); transition: width 0.4s ease, left 0.4s ease; }
.assigned { position: absolute; inset: 0 auto 0 auto; background: color-mix(in srgb, var(--crimson-bright) 65%, transparent); transition: width 0.25s ease, left 0.25s ease; }
.reaches .pending { background: color-mix(in srgb, var(--crimson-bright) 70%, transparent); animation: reaches 1s ease-in-out infinite alternate; }
@keyframes reaches { from { opacity: 0.55; } to { opacity: 1; } }
@media (prefers-reduced-motion: reduce) { .reaches .pending { animation: none; } }
.reaches { box-shadow: 0 0 0 1px var(--crimson-bright); }
.value b { margin-left: 1px; font-weight: 800; color: var(--amber); }
.reaches .value b { color: #fff; }
.value small { font-size: 0.8em; color: rgb(255 255 255 / 0.75); }
</style>
