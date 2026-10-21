<script setup lang="ts">
// One card in the dense list view: thumbnail, name, type and aspect, stats, set.
import CardImage from './CardImage.vue'
import CardStats from './CardStats.vue'
import type { Card } from '@browser/types/card'

const { card, selected = false } = defineProps<{ card: Card; selected?: boolean }>()
</script>

<template>
  <article class="card-row" :class="{ 'card-row--selected': selected }" :data-aspect="card.faction">
    <div class="thumb"><CardImage :card="card" tight /></div>
    <div class="main">
      <h3 class="name">
        <span v-if="card.unique" class="unique" :aria-label="$t('browser.common.unique')">◆</span>
        {{ card.name }}<span v-if="card.subname" class="subname">{{ card.subname }}</span>
      </h3>
      <p v-if="card.traits?.length" class="traits">{{ card.traits.join('. ') }}.</p>
    </div>
    <div class="type">
      <span class="aspect-chip" :data-aspect="card.faction">{{ card.factionName ?? card.faction }}</span>
      <span>{{ card.typeName ?? card.type }}</span>
    </div>
    <div class="stats"><CardStats :card="card" compact /></div>
    <div class="set">
      <span class="truncate">{{ card.setName ?? card.packName }}</span>
      <span class="number">{{ card.packName }} {{ card.position }}</span>
    </div>
  </article>
</template>

<style scoped>
.card-row {
  display: grid;
  grid-template-columns: 2.4rem minmax(10rem, 1.6fr) 13rem minmax(8rem, 1fr) 12rem;
  align-items: center;
  gap: var(--space-3);
  height: 100%;
  padding: 0 var(--space-4);
  border-bottom: 1px solid var(--c-border);
  transition: background var(--transition);
}
.card-row:hover { background: var(--c-surface-2); }
.card-row--selected { background: var(--c-surface-3); box-shadow: inset 3px 0 0 var(--c-accent); }
.thumb { width: 2.4rem; }
.main { min-width: 0; }
.name { display: flex; align-items: baseline; gap: 0.35em; margin: 0; font-family: var(--font-ui); font-size: var(--text-base); font-weight: 600; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.unique { color: var(--chip, var(--c-accent)); font-size: 0.7em; }
.subname { margin-left: 0.4em; font-weight: 400; font-size: 0.85em; color: var(--c-text-faint); }
.traits { margin: 0; font-size: var(--text-xs); color: var(--c-text-faint); font-style: italic; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.type { display: flex; align-items: center; gap: var(--space-2); font-size: var(--text-sm); color: var(--c-text-muted); min-width: 0; }
.set { display: flex; flex-direction: column; min-width: 0; font-size: var(--text-xs); color: var(--c-text-faint); line-height: 1.3; }
.number { font-variant-numeric: tabular-nums; }
@media (width < 60rem) {
  .card-row { grid-template-columns: 2.4rem 1fr auto; }
  .stats, .set { display: none; }
}
</style>
