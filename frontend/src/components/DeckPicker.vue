<script setup lang="ts">
// Choose a deck: a core set starter, one of your decks, or one loaded here from a
// MarvelCDB link for this game alone.
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import DeckImport from '../browser/components/DeckImport.vue'
import DeckLoader from './DeckLoader.vue'
import DeckTile from './DeckTile.vue'
import SegmentedToggle from './SegmentedToggle.vue'
import { useCards } from '../cards'
import { deckFaces, heroFace, type DeckFace, type DeckKey } from '../deckChoice'
import { ASPECTS } from '../browser/types/card'
import type { Deck, DeckList, DeckPreview, DeckSummary, ImportRequest } from '../browser/types/deck'
import type { StarterDeck } from '../server'

// `loaded` is a deck loaded here for this game alone, chosen by the key 'loaded'
const { decks, starters, loaded = null, taken = [] } = defineProps<{
  decks: DeckSummary[]
  starters: StarterDeck[]
  loaded?: DeckFace | null
  /** heroes other seats have, which are unique */
  taken?: string[]
}>()
const model = defineModel<DeckKey>({ required: true })
const emit = defineEmits<{ load: [face: DeckFace, list: DeckList]; saved: [deck: Deck] }>()
// open while a loaded deck is looked over, so the lobby can hold back starting
const loading = defineModel<boolean>('loading', { default: false })
const pending = ref<{ preview: DeckPreview; request: ImportRequest } | null>(null)
const look = (preview: DeckPreview, request: ImportRequest) => {
  pending.value = { preview, request }
  loading.value = true
}
const close = () => {
  pending.value = null
  loading.value = false
}
const { t } = useI18n()

type Tab = 'mine' | 'starter'
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => chosenTab.value ?? (model.value ? (model.value.startsWith('starter:') ? 'starter' : 'mine') : decks.length ? 'mine' : 'starter'))
const tabs = computed(() => [
  { value: 'starter' as const, label: t('newGame.starterTab'), count: starters.length },
  { value: 'mine' as const, label: t('newGame.myDecks'), count: decks.length },
])
const tabFaces = computed(() => [...(loaded ? [loaded] : []), ...deckFaces(starters, decks)].filter((f) => (tab.value === 'starter') === f.starter))

// found by deck or hero name, and by every aspect toggled on
const cards = useCards()
const search = ref('')
const aspects = ref<string[]>([])
const toggleAspect = (aspect: string) => {
  aspects.value = aspects.value.includes(aspect) ? aspects.value.filter((a) => a !== aspect) : [...aspects.value, aspect]
}
const faces = computed(() => {
  const words = search.value.trim().toLowerCase()
  return tabFaces.value.filter(
    (f) =>
      (!words || [f.name ?? '', cards.name(heroFace(f.hero), f.hero)].some((n) => n.toLowerCase().includes(words))) &&
      aspects.value.every((a) => f.aspects.includes(a)),
  )
})
const base = (code: string) => code.replace(/[ab]$/, '')
const isTaken = (face: DeckFace) => taken.map(base).includes(base(face.hero))

// a deck picked folds the list away to it, so what follows is in reach however many
// heroes there are; "Change" opens it again
const picked = ref(false)
const pick = (key: DeckKey) => {
  model.value = key
  picked.value = true
}
const pickedFace = computed(() => [...(loaded ? [loaded] : []), ...deckFaces(starters, decks)].find((f) => f.key === model.value) ?? null)

const use = (face: DeckFace, list: DeckList) => {
  emit('load', face, list)
  pick(face.key)
  chosenTab.value = 'mine'
  close()
}
const saved = (deck: Deck) => {
  emit('saved', deck)
  pick(`deck:${deck.id}`)
  chosenTab.value = 'mine'
  close()
}
watch(model, () => (chosenTab.value = null))
</script>

<template>
  <div class="picker">
    <DeckLoader v-if="pending" :loaded="pending" @use="use" @saved="saved" @cancel="close" />
    <div v-else-if="picked && pickedFace" class="chosen">
      <DeckTile :face="pickedFace" selected compact />
      <button type="button" class="btn" @click="picked = false">{{ $t('newGame.change') }}</button>
    </div>
    <template v-else>
      <SegmentedToggle :options="tabs" :label="$t('newGame.deck')" :model-value="tab" @update:model-value="chosenTab = $event" />
      <DeckImport v-if="tab === 'mine'" :keep="false" @loaded="look" />
      <div v-if="tabFaces.length" class="filters">
        <input v-model="search" class="input search" type="search" :placeholder="$t('newGame.searchDecks')" :aria-label="$t('newGame.searchDecks')" autocomplete="off" />
        <div class="aspects" role="group" :aria-label="$t('newGame.aspects')">
          <button
            v-for="aspect in ASPECTS"
            :key="aspect"
            type="button"
            class="aspect-toggle"
            :data-aspect="aspect"
            :aria-pressed="aspects.includes(aspect)"
            @click="toggleAspect(aspect)"
          >
            {{ $t(`aspects.${aspect}`) }}
          </button>
        </div>
      </div>
      <p v-if="tab === 'mine' && !tabFaces.length" class="none">{{ $t('newGame.noDecks') }}</p>
      <p v-else-if="!faces.length" class="none">{{ $t('newGame.noMatchingDecks') }}</p>
      <div class="deck-picker" role="radiogroup">
        <button
          v-for="face in faces"
          :key="face.key"
          type="button"
          role="radio"
          :aria-checked="model === face.key"
          class="pick"
          :disabled="!!face.problem || (isTaken(face) && model !== face.key)"
          @click="pick(face.key)"
        >
          <DeckTile :face="face" :selected="model === face.key" />
          <span v-if="isTaken(face) && model !== face.key" class="taken">{{ $t('newGame.heroTaken') }}</span>
        </button>
      </div>
    </template>
  </div>
</template>

<style scoped>
.picker { display: grid; gap: 12px; }
.deck-picker { display: grid; grid-template-columns: repeat(auto-fill, minmax(170px, 1fr)); gap: 12px; }
.pick { position: relative; display: grid; padding: 0; border: 0; background: none; font: inherit; cursor: pointer; }
.pick:hover:not(:disabled) :deep(.deck-tile:not(.selected)) { border-color: var(--line-strong); transform: translateY(-2px); }
.pick:disabled { cursor: not-allowed; }
.pick:disabled :deep(.deck-tile) { opacity: 0.4; }
.taken { position: absolute; top: 8px; right: 8px; padding: 2px 8px; border-radius: 999px; background: var(--ink-0); font-size: 0.65rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; color: var(--text-muted); }
.chosen { display: flex; align-items: center; gap: 12px; }
.chosen :deep(.deck-tile) { flex: 1; }
.filters { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }
.search { flex: 1 1 12rem; width: auto; }
.aspects { display: flex; gap: 4px; flex-wrap: wrap; }
.aspect-toggle {
  padding: 5px 10px;
  border: 1px solid color-mix(in srgb, var(--chip) 35%, transparent);
  border-radius: 999px;
  background: none;
  color: var(--text-muted);
  font: inherit;
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  cursor: pointer;
}
.aspect-toggle[aria-pressed='true'] { background: color-mix(in srgb, var(--chip) 24%, transparent); border-color: var(--chip); color: var(--text); }
.none { margin: 0; color: var(--text-faint); font-size: 0.85rem; }
</style>
