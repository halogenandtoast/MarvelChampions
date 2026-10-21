<script setup lang="ts">
// The printed statistics a card has, as the cards abbreviate them, and its resources.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import GameIcon from './GameIcon.vue'
import type { Card } from '@browser/types/card'

const { card, compact = false } = defineProps<{ card: Card; compact?: boolean }>()
const { t } = useI18n()

const stats = computed(() => {
  const rows: { key: string; value: string }[] = []
  const add = (key: string, value: number | null | undefined, suffix = '') => {
    if (value !== null && value !== undefined) rows.push({ key, value: `${value}${suffix}` })
  }
  add('cost', card.cost)
  add('thwart', card.thwart)
  add('attack', card.attack)
  add('defense', card.defense)
  add('recover', card.recover)
  add('scheme', card.scheme)
  add('health', card.health, card.healthPerHero ? '★' : '')
  add('handSize', card.handSize)
  add('threat', card.threat, card.threat !== null && card.threat !== undefined && !card.threatFixed && card.type !== 'side_scheme' ? '/★' : '')
  add('boost', card.boost)
  return rows
})
const resources = computed(() => Object.entries(card.resources ?? {}).flatMap(([name, n]) => Array.from({ length: n ?? 0 }, () => name)))
</script>

<template>
  <ul v-if="stats.length || resources.length" class="stats" :class="{ compact }">
    <li v-for="stat in stats" :key="stat.key" class="stat" :data-stat="stat.key" :title="t(`browser.stats.${stat.key}`)">
      <abbr>{{ t(`browser.stats.short.${stat.key}`) }}</abbr>
      <span class="value">{{ stat.value }}</span>
    </li>
    <li v-if="resources.length" class="stat resources" :title="t('browser.stats.resources')">
      <GameIcon v-for="(r, i) in resources" :key="i" :name="r" />
    </li>
  </ul>
</template>

<style scoped>
.stats { display: flex; flex-wrap: wrap; gap: 6px; margin: 0; padding: 0; list-style: none; }
.stat { display: inline-flex; align-items: baseline; gap: 5px; padding: 4px 9px; border-radius: 8px; background: var(--ink-3); border: 1px solid var(--line); }
abbr { text-decoration: none; font-size: 0.65rem; font-weight: 700; letter-spacing: 0.1em; color: var(--text-faint); }
.value { font-family: var(--font-display); font-size: 1.15rem; font-weight: 700; line-height: 1; }
.stat[data-stat='threat'] .value, .stat[data-stat='scheme'] .value { color: var(--amber); }
.stat[data-stat='health'] .value { color: #9fd8ae; }
.stat[data-stat='attack'] .value { color: var(--attack); }
.stat[data-stat='thwart'] .value, .stat[data-stat='defense'] .value { color: var(--steel-bright); }
.resources { gap: 2px; align-items: center; }
.compact { gap: 4px; flex-wrap: nowrap; }
.compact .stat { padding: 1px 6px; background: none; border: 0; }
.compact .value { font-size: 0.95rem; }
.compact abbr { font-size: 0.58rem; }
</style>
