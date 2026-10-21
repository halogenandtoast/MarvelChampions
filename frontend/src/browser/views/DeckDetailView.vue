<script setup lang="ts">
/**
 * One deck: its identity on both faces, its cards grouped by type. The deck arrives with
 * every card attached, so the page renders before the card index has loaded.
 */
import '@browser/browser.css'
import { computed, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { TriangleAlert } from 'lucide-vue-next'
import CardFace from '../../components/CardFace.vue'
import CardOverlay from '../../components/CardOverlay.vue'
import DeckSection from '@browser/components/DeckSection.vue'
import { fetchDeck } from '@browser/lib/api'
import { messageOf } from '@browser/stores/decks'
import { useCardStore } from '@browser/stores/cards'
import type { Deck, Slot } from '@browser/types/deck'
import { useServerText } from '../../i18n'

const tr = useServerText()
const route = useRoute()
const { t, te } = useI18n()
const store = useCardStore()
void store.load()

const deck = ref<Deck | null>(null)
const error = ref<string | null>(null)
const loading = ref(true)

watch(
  () => route.params.id,
  async (raw) => {
    loading.value = true
    error.value = null
    deck.value = null
    try {
      deck.value = await fetchDeck(Number(raw))
    } catch (err) {
      error.value = messageOf(err)
    } finally {
      loading.value = false
    }
  },
  { immediate: true },
)

// the order a deck list is written in
const TYPE_ORDER = ['ally', 'event', 'support', 'upgrade', 'resource', 'player_side_scheme', 'obligation']
const IDENTITY = ['hero', 'alter_ego']

const byType = computed(() => {
  if (!deck.value) return []
  const groups = new Map<string, Slot[]>()
  for (const slot of deck.value.slots) {
    const type = slot.card?.type ?? 'other'
    if (IDENTITY.includes(type)) continue
    groups.set(type, [...(groups.get(type) ?? []), slot])
  }
  const rank = (type: string) => (TYPE_ORDER.includes(type) ? TYPE_ORDER.indexOf(type) : TYPE_ORDER.length)
  return [...groups.entries()]
    .sort((a, b) => rank(a[0]) - rank(b[0]) || a[0].localeCompare(b[0]))
    .map(([type, slots]) => ({
      type,
      title: te(`browser.cardTypes.${type}`) ? t(`browser.cardTypes.${type}`) : t('browser.deck.other'),
      slots: [...slots].sort((a, b) => (a.card?.cost ?? 99) - (b.card?.cost ?? 99) || (a.card?.name ?? a.code).localeCompare(b.card?.name ?? b.code)),
    }))
})
const size = computed(() => byType.value.reduce((n, g) => n + g.slots.reduce((m, s) => m + s.quantity, 0), 0))
const faces = computed(() => {
  if (!deck.value) return []
  const hero = deck.value.hero.code
  const other = store.cardByCode(hero)?.linkedTo
  return other ? [hero, other] : [hero]
})
</script>

<template>
  <div class="page-container">
    <CardOverlay />
    <main class="page-content card-browser">
      <RouterLink class="button button--ghost back" to="/decks">← {{ $t('browser.deck.allDecks') }}</RouterLink>

      <p v-if="loading" class="notice">{{ $t('browser.common.loading') }}</p>
      <p v-else-if="error" class="notice notice--error">{{ tr(error) }}</p>

      <template v-else-if="deck">
        <header class="head">
          <div class="identity">
            <div v-for="code in faces" :key="code" class="face"><CardFace :code="code" eager /></div>
          </div>
          <div class="head-text">
            <p class="aspects">
              <span v-for="aspect in deck.aspects" :key="aspect" class="aspect-chip" :data-aspect="aspect">{{ $t(`aspects.${aspect}`) }}</span>
            </p>
            <h1>{{ deck.name }}</h1>
            <p class="hero">{{ deck.hero.name }}</p>
            <p class="meta">
              <span>{{ $t('browser.common.cardCount', size) }}</span>
              <a v-if="deck.sourceUrl" :href="deck.sourceUrl" target="_blank" rel="noreferrer noopener">{{ $t('browser.deck.onMarvelcdb') }} ↗</a>
            </p>
            <p v-if="deck.problem" class="problem"><TriangleAlert aria-hidden="true" />{{ tr(deck.problem) }}</p>
            <RouterLink v-if="!deck.problem" :to="{ path: '/new-game', query: { deck: deck.id } }" class="btn primary">{{ $t('browser.deck.play') }}</RouterLink>
          </div>
        </header>

        <div class="columns">
          <DeckSection v-for="group in byType" :key="group.type" :title="group.title" :slots="group.slots" />
        </div>

        <p v-if="deck.description" class="description">{{ deck.description }}</p>
      </template>
    </main>
  </div>
</template>

<style scoped>
.back { margin-bottom: var(--space-4); }
.head { display: flex; gap: var(--space-6); align-items: flex-start; margin-bottom: var(--space-6); padding: var(--space-5); border-radius: 18px; background: linear-gradient(120deg, color-mix(in srgb, var(--steel) 14%, transparent), transparent 60%), var(--c-surface); border: 1px solid var(--c-border); }
.identity { display: flex; gap: var(--space-3); flex: none; }
.face { width: 10rem; aspect-ratio: 63 / 88; border-radius: 6% / 4.3%; overflow: hidden; box-shadow: 0 10px 30px rgb(0 0 0 / 0.5); }
.face + .face { width: 7.5rem; opacity: 0.9; }
.head-text { display: flex; flex-direction: column; align-items: flex-start; gap: var(--space-2); min-width: 0; }
.aspects { display: flex; gap: 6px; margin: 0; }
h1 { margin: 0; font-size: 2.6rem; line-height: 1; text-transform: uppercase; text-wrap: balance; }
.hero { margin: 0; color: var(--c-text-muted); font-size: var(--text-lg); }
.meta { display: flex; flex-wrap: wrap; gap: var(--space-3); margin: 0; color: var(--c-text-faint); font-size: var(--text-sm); }
.meta a:hover { color: var(--title); }
.problem { display: flex; align-items: center; gap: 8px; margin: 0; padding: 8px 12px; border-radius: 8px; border: 1px solid rgb(214 80 88 / 0.5); color: #f0a4a9; font-size: var(--text-sm); }
.problem svg { width: 16px; height: 16px; flex: none; }
.columns { columns: 19rem; column-gap: var(--space-6); }
.columns > * { break-inside: avoid; margin-bottom: var(--space-5); }
.description { margin-top: var(--space-5); padding-top: var(--space-4); border-top: 1px solid var(--c-border); color: var(--c-text-muted); font-size: var(--text-sm); white-space: pre-wrap; }
.notice { padding: var(--space-4); border: 1px dashed var(--c-border); border-radius: var(--radius); color: var(--c-text-muted); text-align: center; }
.notice--error { border-style: solid; border-color: var(--c-danger); color: var(--c-danger); }
@media (max-width: 700px) {
  .head { flex-direction: column; }
}
</style>
