<script setup lang="ts">
// What is being played, over its settings and its lobby: the scenario's villains or the
// campaign's box, its name, and what was chosen for it.
import { useCards } from '../cards'
import { campaignArt } from '../campaigns'
import type { CampaignKind } from '../server'

const { portraits = [], campaign, name, details } = defineProps<{ portraits?: string[]; campaign?: CampaignKind; name: string; details: string[] }>()
const cards = useCards()
const art = (code: string) => cards.sources(code).map((s) => `url('${s}')`).join(', ')
</script>

<template>
  <header class="setup-bar">
    <span v-if="portraits.length" class="thumb">
      <span v-for="code in portraits" :key="code" class="portrait" :class="{ several: portraits.length > 1, 'scheme-art': cards.landscape(code) }" :style="{ backgroundImage: art(code) }" />
    </span>
    <span v-else-if="campaign" class="thumb box" :style="{ backgroundImage: `url('${campaignArt[campaign]}')` }" />
    <span class="text">
      <span class="name">{{ name }}</span>
      <span class="details">{{ details.join(' · ') }}</span>
    </span>
    <slot />
  </header>
</template>

<style scoped>
.setup-bar { display: flex; align-items: center; gap: 16px; padding: 12px 16px 12px 12px; border-radius: 16px; background: var(--ink-2); border: 1px solid var(--line); }
.thumb { flex: none; display: grid; grid-auto-flow: column; grid-auto-columns: 1fr; width: 112px; height: 70px; border-radius: 10px; overflow: hidden; }
.thumb .portrait { height: 100%; }
.thumb .several { background-size: auto 210%; background-position: 72% 12%; }
.box { width: 66px; background: center / contain no-repeat; }
.text { display: grid; gap: 2px; min-width: 0; margin-right: auto; }
.name { font-family: var(--font-display); font-size: 1.6rem; font-weight: 800; line-height: 1; text-transform: uppercase; color: var(--title); }
.details { font-size: 0.85rem; color: var(--text-muted); }
</style>
