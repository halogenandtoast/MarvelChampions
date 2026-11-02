/**
 * The card database and the browse state that reads from it. The index is fetched once;
 * everything after that is a computation over an in-memory array.
 */
import { computed, ref, shallowRef } from 'vue'
import { defineStore } from './store'
import { activeFilterCount, emptyFilters, filterCards, isEmptyFilters, sortCards, type CardFilters, type SortField } from '@browser/lib/search'
import type { Card, CardIndex, Pack } from '@browser/types/card'

export type LoadState = 'idle' | 'loading' | 'ready' | 'error'

// ponytail: a static file from the importer, served as is (the server reads the same one)
async function fetchCardIndex(): Promise<CardIndex> {
  const response = await fetch('/data/cards.json')
  if (!response.ok) throw new Error(`${response.status} ${response.statusText}`.trim())
  return response.json()
}

export const useCardStore = defineStore('cards', () => {
  // shallow: the array is large and never changes once loaded
  const all = shallowRef<Card[]>([])
  const packs = shallowRef<Pack[]>([])
  const state = ref<LoadState>('idle')
  const error = ref<string | null>(null)

  const filters = ref<CardFilters>(emptyFilters())
  const sortField = ref<SortField>('set')
  const sortDescending = ref(false)
  /** Cards printed as the hidden back of another (alter-egos, scheme B sides) are browsed with their front. */
  const showHidden = ref(false)

  let inFlight: Promise<void> | null = null
  function load(): Promise<void> {
    if (state.value === 'ready') return Promise.resolve()
    if (inFlight) return inFlight
    state.value = 'loading'
    inFlight = fetchCardIndex()
      .then((index) => {
        all.value = index.cards
        packs.value = [...index.packs].sort((a, b) => a.order - b.order)
        state.value = 'ready'
      })
      .catch((err: unknown) => {
        error.value = err instanceof Error ? err.message : String(err)
        state.value = 'error'
      })
      .finally(() => (inFlight = null))
    return inFlight
  }

  const byCode = computed(() => {
    const index = new Map(all.value.map((c) => [c.code, c]))
    for (const card of all.value) for (const alias of card.aliases ?? []) {
      if (!index.has(alias)) index.set(alias, card)
    }
    return index
  })
  const cardByCode = (code: string | null | undefined): Card | undefined => (code ? byCode.value.get(code) : undefined)

  /** A fold-out identity's third side (Ant-Man's Giant, 12001c beside 12001a), browsed with its A side. */
  const thirdFace = (card: Card): Card | undefined => (/a$/.test(card.code) ? cardByCode(card.code.replace(/a$/, 'c')) : undefined)
  const isThirdFace = (card: Card) => /c$/.test(card.code) && byCode.value.has(card.code.replace(/c$/, 'a'))

  /** MarvelCDB's older single record of a double-sided card it also lists as linked A and B sides (Kang's 11007 beside 11007a/11007b). */
  const isSplitCard = (card: Card) => !/[a-z]$/.test(card.code) && !!cardByCode(`${card.code}a`)?.linkedTo

  /** What the browser lists: one entry a card, its other faces reached from it. */
  const cards = computed(() =>
    all.value.filter((c) => !c.duplicateOf && !isSplitCard(c) && (showHidden.value || (!c.hidden && !isThirdFace(c)))),
  )
  const results = computed(() => sortCards(filterCards(cards.value, filters.value), sortField.value, sortDescending.value))

  /** Facet values present in the database, with how many cards carry each. */
  const facets = computed(() => {
    const count = (values: (string | undefined)[]) => {
      const m = new Map<string, number>()
      for (const v of values) if (v) m.set(v, (m.get(v) ?? 0) + 1)
      return m
    }
    const sets = new Map<string, { name: string; count: number }>()
    for (const c of cards.value) {
      if (!c.setCode || !c.setName) continue
      const seen = sets.get(c.setCode)
      sets.set(c.setCode, { name: c.setName, count: (seen?.count ?? 0) + 1 })
    }
    return {
      types: count(cards.value.map((c) => c.type)),
      factions: count(cards.value.map((c) => c.faction)),
      traits: [...count(cards.value.flatMap((c) => c.traits ?? []))].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])),
      sets: [...sets].sort((a, b) => a[1].name.localeCompare(b[1].name)),
      packs: count(cards.value.map((c) => c.packCode)),
    }
  })

  const hasFilters = computed(() => !isEmptyFilters(filters.value))
  const filterCount = computed(() => activeFilterCount(filters.value))

  function resetFilters(): void {
    filters.value = emptyFilters()
  }

  function toggle(key: 'types' | 'factions' | 'packs' | 'sets' | 'traits', value: string): void {
    const list = filters.value[key]
    const i = list.indexOf(value)
    if (i === -1) list.push(value)
    else list.splice(i, 1)
  }

  return { all, cards, packs, state, error, filters, sortField, sortDescending, showHidden, load, cardByCode, thirdFace, results, facets, hasFilters, filterCount, resetFilters, toggle }
})
