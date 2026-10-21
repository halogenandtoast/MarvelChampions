<script setup lang="ts">
// A resource or game icon, as card text writes them: [energy], [mental], [physical],
// [wild], [boost], [star], [per_hero]... drawn in the resource's colour.
import { computed, type Component } from 'vue'
import { Biohazard, Brain, ChevronsUp, Coins, Diamond, Flame, HandFist, Sparkles, Star, TriangleAlert, User, Volume2, Zap } from 'lucide-vue-next'

const { name, label = null } = defineProps<{ name: string; label?: string | null }>()
const icons: Record<string, Component> = {
  energy: Zap,
  mental: Brain,
  physical: HandFist,
  wild: Sparkles,
  boost: Flame,
  star: Star,
  per_hero: User,
  per_player: User,
  crisis: TriangleAlert,
  acceleration: ChevronsUp,
  amplify: Volume2,
  hazard: Biohazard,
  cost: Coins,
  unique: Diamond,
}
const icon = computed(() => icons[name] ?? null)
</script>

<template>
  <component :is="icon" v-if="icon" class="game-icon" :class="name" role="img" :aria-label="label ?? name" :title="label ?? name" />
  <span v-else class="game-icon word" :class="name">{{ label ?? name }}</span>
</template>

<style scoped>
.game-icon { display: inline-block; width: 1.05em; height: 1.05em; vertical-align: -0.17em; stroke-width: 2.4; }
.energy { color: #f0c64a; }
.mental { color: #6aa8e8; }
.physical { color: #e0636a; }
.wild { color: #66c58a; }
.boost { color: #f08a4b; }
.star { color: #f0c64a; fill: currentColor; }
.per_hero, .per_player { color: var(--steel-bright); }
.crisis, .hazard { color: var(--amber); }
.acceleration { color: var(--crimson-bright); }
.word { width: auto; height: auto; font-size: 0.8em; font-weight: 800; text-transform: uppercase; letter-spacing: 0.06em; vertical-align: baseline; }
</style>
