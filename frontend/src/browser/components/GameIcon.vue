<script setup lang="ts">
// A resource or game icon, as card text writes them: [energy], [mental], [physical],
// [wild], [boost], [star], [per_hero]... drawn in the resource's colour.
import { computed, type Component } from 'vue'
import { Coins, Flame, Volume2 } from 'lucide-vue-next'

const { name, label = null } = defineProps<{ name: string; label?: string | null }>()
// The game's own icons are letters in the Marvel LCG glyph font; lucide draws the rest.
const glyphs: Record<string, string> = {
  energy: 'e',
  mental: 'm',
  physical: 'p',
  wild: 'w',
  star: 'S',
  per_hero: 'P',
  per_player: 'P',
  crisis: 'C',
  acceleration: 'A',
  hazard: 'H',
  unique: 'U',
}
const icons: Record<string, Component> = {
  boost: Flame,
  amplify: Volume2,
  cost: Coins,
}
const glyph = computed(() => glyphs[name] ?? null)
const icon = computed(() => icons[name] ?? null)
</script>

<template>
  <span v-if="glyph" class="game-icon glyph" :class="name" role="img" :aria-label="label ?? name" :title="label ?? name">{{ glyph }}</span>
  <component :is="icon" v-else-if="icon" class="game-icon" :class="name" role="img" :aria-label="label ?? name" :title="label ?? name" />
  <span v-else class="game-icon word" :class="name">{{ label ?? name }}</span>
</template>

<style scoped>
@font-face { font-family: "Marvel LCG Glyphs"; src: url('../../assets/marvel-glyphs.ttf') format('truetype'); font-display: block; }
.game-icon { display: inline-block; width: 1.05em; height: 1.05em; vertical-align: -0.17em; stroke-width: 2.4; }
.glyph { font-family: "Marvel LCG Glyphs"; font-style: normal; font-weight: normal; line-height: 1; text-align: center; vertical-align: -0.12em; }
.energy { --resource: #e2b953; --resource-tint: #76642c; color: var(--resource); }
.mental { --resource: #505aa1; --resource-tint: #b2b3d5; color: var(--resource); }
.physical { --resource: #8a2e1e; --resource-tint: #d59277; color: var(--resource); }
.wild { --resource: #2e5227; --resource-tint: #60944b; color: var(--resource); }
.boost { color: #f08a4b; }
.star { color: #f0c64a; }
.per_hero, .per_player { color: var(--steel-bright); }
.crisis, .hazard { color: var(--amber); }
.acceleration { color: var(--crimson-bright); }
.word { width: auto; height: auto; font-size: 0.8em; font-weight: 800; text-transform: uppercase; letter-spacing: 0.06em; vertical-align: baseline; }
</style>
