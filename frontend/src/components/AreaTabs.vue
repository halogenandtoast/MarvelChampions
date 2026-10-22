<script setup lang="ts">
// A tab a game area, when the players are split into several: your own first, each named
// for the players in it, and lit while a choice you have to make is about something in it.
import type { AreaView } from '../server'

defineProps<{
  areas: AreaView[]
  shown: number | null
  own: number | null
  /** the areas a choice open to you is about */
  lit: number[]
  names: Record<string, string>
  /** each area's number, as the game made them */
  numbers: Record<number, number>
}>()
const emit = defineEmits<{ show: [areaId: number] }>()
</script>

<template>
  <nav class="area-tabs" role="tablist" :aria-label="$t('game.areas.label')">
    <button
      v-for="area in areas"
      :key="area.id"
      type="button"
      role="tab"
      class="tab"
      :class="{ shown: area.id === shown, lit: lit.includes(area.id) }"
      :aria-selected="area.id === shown"
      @click="emit('show', area.id)"
    >
      <span class="name">{{ $t('game.areas.area', { n: numbers[area.id] ?? '' }) }}<span v-if="area.id === own" class="yours">{{ $t('game.areas.yours') }}</span></span>
      <span class="meta">{{ area.players.map((p) => names[p]).filter(Boolean).join(', ') || $t('game.areas.empty') }}</span>
    </button>
  </nav>
</template>

<style scoped>
.area-tabs { display: flex; gap: 6px; overflow-x: auto; flex: none; }
.tab {
  display: flex;
  flex-direction: column;
  gap: 1px;
  padding: 5px 12px;
  border: 1px solid var(--line);
  border-radius: 10px;
  background: var(--ink-2);
  color: var(--text-muted);
  font: inherit;
  text-align: left;
  white-space: nowrap;
  cursor: pointer;
  transition: border-color 0.15s, background 0.15s;
}
.tab:hover { background: var(--ink-3); color: var(--text); }
.tab.shown { border-color: rgb(184 50 58 / 0.7); background: linear-gradient(rgb(184 50 58 / 0.16), rgb(184 50 58 / 0.05)), var(--ink-2); color: var(--text); }
.tab.lit:not(.shown) { border-color: var(--interactive); box-shadow: 0 0 10px var(--interactive-glow); }
.name { font-weight: 700; font-size: 0.8rem; color: var(--text); }
.yours { margin-left: 6px; font-size: 0.6rem; font-weight: 800; letter-spacing: 0.1em; text-transform: uppercase; color: var(--steel-bright); }
.meta { font-size: 0.7rem; color: var(--text-faint); }
</style>
