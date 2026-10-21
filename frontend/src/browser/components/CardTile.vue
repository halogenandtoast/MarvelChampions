<script setup lang="ts">
// One card in the grid view: the scan, with its name underneath.
import CardImage from './CardImage.vue'
import type { Card } from '@browser/types/card'

const { card, selected = false } = defineProps<{ card: Card; selected?: boolean }>()
</script>

<template>
  <article class="tile" :class="{ 'tile--selected': selected }" :data-aspect="card.faction">
    <CardImage :card="card" />
    <div class="caption">
      <span class="name truncate">
        <span v-if="card.unique" class="unique" :aria-label="$t('browser.common.unique')">◆</span>
        {{ card.name }}
      </span>
      <span class="set truncate">{{ card.typeName }} · {{ card.setName ?? card.packName }}</span>
    </div>
  </article>
</template>

<style scoped>
.tile { display: flex; flex-direction: column; gap: var(--space-2); border-radius: var(--radius); transition: transform var(--transition), filter var(--transition); }
.tile:hover { transform: translateY(-3px); filter: brightness(1.08); }
.tile--selected :deep(.frame) { outline: 2px solid var(--c-accent); outline-offset: 2px; }
.caption { display: flex; flex-direction: column; min-width: 0; line-height: 1.3; }
.name { font-size: var(--text-sm); font-weight: 600; }
.unique { color: var(--chip, var(--c-accent)); font-size: 0.7em; }
.set { font-size: var(--text-xs); color: var(--c-text-faint); }
</style>
