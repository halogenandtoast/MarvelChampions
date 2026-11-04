<script setup lang="ts">
import { Check } from 'lucide-vue-next'
import { useCards } from '../cards'
import { scenarioPortraits } from '../scenarios'

const { code, title, subtitle = '', selected = false, completed = false, lost = false, selectable = false } = defineProps<{
  code: string
  title: string
  subtitle?: string
  selected?: boolean
  completed?: boolean
  lost?: boolean
  selectable?: boolean
}>()
const cards = useCards()
const art = (code: string) => cards.sources(code).map(source => `url('${source}')`).join(', ')
</script>

<template>
  <component :is="selectable ? 'button' : 'div'" :type="selectable ? 'button' : undefined" class="scenario-card" :class="{ selected, completed, lost, selectable }">
    <span class="art-strip">
      <span v-for="portrait in scenarioPortraits(code)" :key="portrait" class="art portrait" :class="{ 'scheme-art': cards.landscape(portrait) }" :data-code="portrait" :style="{ backgroundImage: art(portrait) }" />
    </span>
    <span class="villain-name">{{ title }}</span>
    <span v-if="subtitle" class="villain-sets">{{ subtitle }}</span>
    <Check v-if="completed || (selectable && selected)" class="tick" :class="{ completed }" aria-hidden="true" />
  </component>
</template>

<style scoped>
.scenario-card {
  position: relative;
  width: 100%;
  height: 100%;
  min-width: 0;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  padding: 0 0 12px;
  border: 2px solid var(--line);
  border-radius: 14px;
  background: var(--ink-3);
  color: var(--text);
  font: inherit;
  text-align: left;
  overflow: hidden;
  cursor: default;
  transition: border-color 0.15s, transform 0.15s;
}
.scenario-card.selectable:hover { transform: translateY(-2px); border-color: var(--line-strong); }
.scenario-card.selected { border-color: var(--crimson); box-shadow: 0 0 0 4px rgb(184 50 58 / 0.2), 0 12px 30px rgb(184 50 58 / 0.15); }
.art { position: relative; width: 100%; aspect-ratio: 16 / 10; }
.art::after { content: ''; position: absolute; inset: 0; background: linear-gradient(transparent 50%, var(--ink-3)); }
.villain-name { margin: -18px 14px 0; position: relative; font-family: var(--font-display); font-size: 1.9rem; font-weight: 800; line-height: 1; text-transform: uppercase; }
.villain-sets { margin: 4px 14px 0; font-size: 0.75rem; color: var(--text-faint); }
.tick { position: absolute; top: 10px; right: 10px; width: 26px; height: 26px; padding: 4px; border-radius: 50%; background: var(--crimson); color: #fff; }

.art-strip { display: grid; grid-auto-flow: column; grid-auto-columns: 1fr; width: 100%; aspect-ratio: 16 / 10; }
.art-strip .art { aspect-ratio: auto; height: 100%; }
/* Landscape schemes keep their illustration on the right of the rules panel. */
.art-strip .art[data-code='60121b'],
.art-strip .art[data-code='60128b'],
.art-strip .art[data-code='60134b'],
.art-strip .art[data-code='60142b'],
.art-strip .art[data-code='60151b'],
.art-strip .art[data-code='65001b'] { background-size: 230% auto; background-position: 100% 45%; }
.art-strip .art[data-code='40136'],
.art-strip .art[data-code='40137'],
.art-strip .art[data-code='40138'] { background-position: 95% calc(19% + 16px); }
.art-strip .art[data-code='16103'] { background-position: 95% 12%; }
.art-strip .art[data-code='45101b'] { background-position: 95% calc(19% + 16px); }
.art-strip .art:not(:only-child) { background-size: auto 210%; background-position: 72% 12%; }
.art-strip .art[data-code='45081a'] { background-position: calc(72% - 24px) calc(12% - 8px); }
.art-strip .art[data-code='45082a'] { background-position: calc(72% + 24px) calc(12% - 8px); }
.art-strip .art[data-code='45083a'],
.art-strip .art[data-code='45084a'] { background-position: 72% calc(12% - 8px); }
.art-strip .art[data-code='27096'],
.art-strip .art[data-code='27098'] { background-position: 72% calc(12% - 16px); }
.art-strip .art[data-code='27099'],
.art-strip .art[data-code='27097'],
.art-strip .art[data-code='27095'] { background-position: calc(72% + 12px) calc(12% - 16px); }
.art-strip .art[data-code='27094'] { background-position: calc(72% + 80px) calc(12% - 16px); }
.art-strip .art[data-code='27096'] { background-position: calc(72% + 64px) calc(12% - 16px); }
.art-strip .art[data-code='07002'] { background-position: calc(72% + 16px) calc(12% - 8px); }
.art-strip .art[data-code='07017'] { background-position: calc(72% + 32px) calc(12% - 8px); }
.art-strip .art[data-code='07032'] { background-position: calc(72% + 48px) calc(12% - 8px); }
.art-strip .art[data-code='07046'] { background-position: 72% calc(12% - 8px); }

.scenario-card.selectable { cursor: pointer; }
.scenario-card.lost { border-color: var(--danger); }
.tick.completed { background: var(--health); }
</style>
