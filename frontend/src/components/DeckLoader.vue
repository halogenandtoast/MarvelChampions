<script setup lang="ts">
// A deck loaded from a link or file, looked over before it is played: kept in your decks
// only when asked, so a deck to try needs no trip to the decks page.
import { computed, ref } from 'vue'
import DeckTile from './DeckTile.vue'
import { importDeck } from '../browser/lib/api'
import type { Deck, DeckList, DeckPreview, ImportRequest } from '../browser/types/deck'
import type { DeckFace } from '../deckChoice'
import { useServerText } from '../i18n'

const { loaded } = defineProps<{ loaded: { preview: DeckPreview; request: ImportRequest } }>()
const emit = defineEmits<{ use: [face: DeckFace, list: DeckList]; saved: [deck: Deck]; cancel: [] }>()
const tr = useServerText()

const save = ref(false)
const busy = ref(false)
const error = ref<string | null>(null)

const face = computed((): DeckFace => {
  const { preview } = loaded
  return { key: 'loaded', starter: false, hero: preview.hero.code, name: preview.name, aspects: preview.aspects, problem: preview.problem }
})

const use = async () => {
  const { preview, request } = loaded
  if (!save.value) return emit('use', face.value, preview.list!)
  busy.value = true
  error.value = null
  try {
    emit('saved', await importDeck({ ...request, save: true }))
  } catch (err) {
    error.value = err instanceof Error ? err.message : String(err)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="deck-loader">
    <div class="preview">
      <DeckTile :face="face" />
      <dl class="details">
        <div><dt>{{ $t('newGame.loader.hero') }}</dt><dd>{{ loaded.preview.hero.name }}</dd></div>
        <div><dt>{{ $t('newGame.loader.cards') }}</dt><dd>{{ loaded.preview.cardCount }}</dd></div>
      </dl>
    </div>
    <p v-if="loaded.preview.problem" class="problem" role="alert">{{ tr(loaded.preview.problem) }}</p>
    <label class="switch">
      <input v-model="save" type="checkbox" role="switch" />
      <span class="track" aria-hidden="true" />
      {{ $t('newGame.loader.save') }}
    </label>
    <p v-if="error" class="problem" role="alert">{{ tr(error) }}</p>
    <div class="actions">
      <button type="button" class="btn" @click="emit('cancel')">{{ $t('common.back') }}</button>
      <button type="button" class="btn primary" :disabled="busy || !loaded.preview.list" @click="use">{{ $t('newGame.loader.use') }}</button>
    </div>
  </div>
</template>

<style scoped>
.deck-loader { display: grid; gap: 12px; }
.preview { display: grid; grid-template-columns: 220px 1fr; gap: 16px; align-items: start; }
.details { display: grid; gap: 6px; margin: 0; }
.details div { display: flex; gap: 8px; }
.details dt { color: var(--text-muted); }
.details dd { margin: 0; font-weight: 700; }
.problem { margin: 0; color: var(--danger); }
.actions { display: flex; gap: 10px; }
.actions .btn { flex: 1; }
</style>
