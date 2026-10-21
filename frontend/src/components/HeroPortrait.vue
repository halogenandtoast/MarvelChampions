<script setup lang="ts">
// A hero's art, cropped from the top of their card, over a bar in their aspect's colour.
import { computed } from 'vue'
import { useCards } from '../cards'
import { heroFace } from '../deckChoice'

const { code, aspects = [] } = defineProps<{ code: string; aspects?: string[] }>()
const cards = useCards()
const image = computed(() => cards.sources(heroFace(code)).map((s) => `url('${s}')`).join(', '))
const bar = computed(() => {
  const colours = (aspects.length ? aspects : ['hero']).map((a) => `var(--${a}, var(--basic))`)
  return colours.length > 1 ? `linear-gradient(90deg, ${colours.join(', ')})` : colours[0]
})
</script>

<template>
  <span class="hero-portrait portrait" :style="{ backgroundImage: image, '--bar': bar }" />
</template>

<style scoped>
.hero-portrait { position: relative; display: block; border-radius: inherit; }
.hero-portrait::after { content: ''; position: absolute; inset: auto 0 0 0; height: 4px; background: var(--bar); }
</style>
