<script setup lang="ts">
// A deck shown by its hero's portrait, named underneath, its aspects as chips.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from '../cards'
import { heroFace, type DeckFace } from '../deckChoice'
import { useServerText } from '../i18n'
import HeroPortrait from './HeroPortrait.vue'

const { face, selected = false } = defineProps<{ face: DeckFace; selected?: boolean }>()
const cards = useCards()
const { t } = useI18n()
const tr = useServerText()
const heroName = computed(() => cards.name(heroFace(face.hero), face.hero))
</script>

<template>
  <span class="deck-tile" :class="{ selected, unplayable: face.problem }" :title="face.problem ? tr(face.problem) : undefined">
    <HeroPortrait class="art" :code="face.hero" :aspects="face.aspects" />
    <span class="text">
      <span class="deck-name">{{ face.name ?? heroName }}</span>
      <span class="deck-detail">
        <span v-for="aspect in face.aspects" :key="aspect" class="aspect-chip" :data-aspect="aspect">{{ t(`aspects.${aspect}`) }}</span>
        <span v-if="face.starter" class="kind">{{ t('newGame.starterDeck') }}</span>
        <span v-else class="kind">{{ heroName }}</span>
      </span>
    </span>
  </span>
</template>

<style scoped>
.deck-tile {
  display: flex;
  flex-direction: column;
  min-width: 0;
  border: 2px solid var(--line);
  border-radius: 12px;
  overflow: hidden;
  background: var(--ink-3);
  text-align: left;
  transition: border-color 0.15s, transform 0.15s;
}
.deck-tile.selected { border-color: var(--crimson); box-shadow: 0 0 0 3px rgb(184 50 58 / 0.25); }
.deck-tile.unplayable { opacity: 0.6; }
.art { aspect-ratio: 16 / 9; border-radius: 0; }
.text { display: flex; flex-direction: column; min-width: 0; }
.deck-name { padding: 8px 10px 0; font-family: var(--font-display); font-size: 1.1rem; font-weight: 700; color: var(--text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.deck-detail { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; padding: 4px 10px 10px; }
.kind { font-size: 0.72rem; color: var(--text-faint); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
</style>
