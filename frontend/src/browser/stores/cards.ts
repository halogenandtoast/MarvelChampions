/**
 * The card database and the browse state that reads from it. The English index is
 * fetched once; a locale only overlays display fields, keyed by stable card code.
 */
import { computed, ref, shallowRef, watch } from 'vue'
import { defineStore } from './store'
import { activeFilterCount, emptyFilters, filterCards, isEmptyFilters, sortCards, type CardFilters, type SortField } from '@browser/lib/search'
import type { Card, CardIndex, Errata, ErrataIndex, Pack } from '@browser/types/card'
import { activeLocale } from '../../locales/activeLocale'
import type { SupportedLocale } from '../../locales/messages'

export type LoadState = 'idle' | 'loading' | 'ready' | 'error'

export interface CardLocalization {
  name?: string
  text?: string
  flavor?: string
  traits?: string[]
  typeName?: string
  factionName?: string
  packName?: string
  setName?: string
  backName?: string
  backText?: string
  backFlavor?: string
}

interface CardLocalizationIndex {
  version: number
  locale: SupportedLocale
  sourceGeneratedAt: string
  packs: Record<string, string>
  cards: Record<string, CardLocalization>
}

type LocalizedLocale = Exclude<SupportedLocale, 'en'>

// ponytail: a static file from the importer, served as is (the server reads the same one)
async function fetchCardIndex(): Promise<CardIndex> {
  const response = await fetch('/data/cards.json')
  if (!response.ok) throw new Error(`${response.status} ${response.statusText}`.trim())
  return response.json()
}

async function fetchCardLocalization(locale: SupportedLocale): Promise<CardLocalizationIndex> {
  const response = await fetch(`/data/cards.${locale}.json`)
  if (!response.ok) throw new Error(`${response.status} ${response.statusText}`.trim())
  return response.json()
}

// The Rules Reference errata, beside the index; a card without one, or no file at all, has none.
async function fetchErrata(locale: SupportedLocale = 'en'): Promise<Record<string, Errata>> {
  try {
    const response = await fetch(locale === 'en' ? '/data/errata.json' : `/data/errata.${locale}.json`)
    return response.ok ? ((await response.json()) as ErrataIndex).cards : {}
  } catch {
    return {}
  }
}

export const useCardStore = defineStore('cards', () => {
  // The English records remain intact for game logic and bilingual search.
  const rawAll = shallowRef<Card[]>([])
  const rawPacks = shallowRef<Pack[]>([])
  const localizedIndex = shallowRef<CardLocalizationIndex | null>(null)
  const errata = shallowRef<Record<string, Errata>>({})
  const localizedErrata = shallowRef<Record<string, Errata>>({})
  const state = ref<LoadState>('idle')
  const error = ref<string | null>(null)

  const filters = ref<CardFilters>(emptyFilters())
  const sortField = ref<SortField>('set')
  const sortDescending = ref(false)
  /** Cards printed as the hidden back of another (alter-egos, scheme B sides) are browsed with their front. */
  const showHidden = ref(false)

  const localizationCache = new Map<LocalizedLocale, CardLocalizationIndex>()
  const localizationRequests = new Map<LocalizedLocale, Promise<CardLocalizationIndex>>()

  async function updateLocalization(locale: SupportedLocale): Promise<void> {
    if (locale === 'en') {
      localizedIndex.value = null
      return
    }
    const cached = localizationCache.get(locale)
    if (cached) {
      localizedIndex.value = cached
      return
    }
    let request = localizationRequests.get(locale)
    if (!request) {
      request = fetchCardLocalization(locale).then((index) => {
        localizationCache.set(locale, index)
        return index
      })
      localizationRequests.set(locale, request)
    }
    try {
      const index = await request
      // A slower response for the previous language must not replace the new one.
      if (activeLocale.value === locale) localizedIndex.value = index
    } catch (err) {
      // Card localization is optional: never take down the usable English index.
      if (activeLocale.value === locale) localizedIndex.value = null
      console.warn(`Could not load ${locale} card text; using English cards.`, err)
    } finally {
      if (localizationRequests.get(locale) === request) localizationRequests.delete(locale)
    }
  }

  const errataCache = new Map<LocalizedLocale, Record<string, Errata>>()
  const errataRequests = new Map<LocalizedLocale, Promise<Record<string, Errata>>>()
  async function updateErrata(locale: SupportedLocale): Promise<void> {
    if (locale === 'en') {
      localizedErrata.value = {}
      return
    }
    const cached = errataCache.get(locale)
    if (cached) {
      localizedErrata.value = cached
      return
    }
    let request = errataRequests.get(locale)
    if (!request) {
      request = fetchErrata(locale).then((entries) => {
        errataCache.set(locale, entries)
        return entries
      })
      errataRequests.set(locale, request)
    }
    const entries = await request
    if (activeLocale.value === locale) localizedErrata.value = entries
    if (errataRequests.get(locale) === request) errataRequests.delete(locale)
  }

  watch(activeLocale, (locale) => {
    void updateLocalization(locale)
    void updateErrata(locale)
  }, { immediate: true })

  let inFlight: Promise<void> | null = null
  function load(): Promise<void> {
    if (state.value === 'ready') return Promise.resolve()
    if (inFlight) return inFlight
    state.value = 'loading'
    inFlight = Promise.all([fetchCardIndex(), fetchErrata()])
      .then(([index, cardErrata]) => {
        rawAll.value = index.cards
        rawPacks.value = [...index.packs].sort((a, b) => a.order - b.order)
        errata.value = cardErrata
        state.value = 'ready'
      })
      .catch((err: unknown) => {
        error.value = err instanceof Error ? err.message : String(err)
        state.value = 'error'
      })
      .finally(() => (inFlight = null))
    return inFlight
  }

  const all = computed<Card[]>(() => {
    const localization = activeLocale.value === 'en' ? null : localizedIndex.value
    if (!localization) return rawAll.value
    return rawAll.value.map((card) => ({ ...card, ...localization.cards[card.code] }))
  })
  const packs = computed<Pack[]>(() => {
    const localization = activeLocale.value === 'en' ? null : localizedIndex.value
    if (!localization) return rawPacks.value
    return rawPacks.value.map((pack) => ({ ...pack, name: localization.packs[pack.code] ?? pack.name }))
  })

  const byCode = computed(() => {
    const index = new Map(all.value.map((c) => [c.code, c]))
    for (const card of all.value) for (const alias of card.aliases ?? []) {
      if (!index.has(alias)) index.set(alias, card)
    }
    return index
  })
  const rawByCode = computed(() => new Map(rawAll.value.map((card) => [card.code, card])))
  const cardByCode = (code: string | null | undefined): Card | undefined => (code ? byCode.value.get(code) : undefined)
  const bySet = computed(() => {
    const index = new Map<string, Card>()
    for (const card of all.value) if (card.setCode && !index.has(card.setCode)) index.set(card.setCode, card)
    return index
  })
  const cardBySet = (code: string): Card | undefined => bySet.value.get(code)

  /** A card's errata, by its own code or the face it was printed with (01029a beside 01029). */
  const errataFor = (code: string | null | undefined): Errata | undefined => {
    if (!code) return undefined
    const lettered = /[a-z]$/.test(code)
    const relatedCode = lettered ? code.slice(0, -1) : `${code}a`
    return localizedErrata.value[code] ?? localizedErrata.value[relatedCode]
      ?? errata.value[code] ?? errata.value[relatedCode]
  }

  /** A fold-out identity's third side (Ant-Man's Giant, 12001c beside 12001a), browsed with its A side. */
  const thirdFace = (card: Card): Card | undefined => (/a$/.test(card.code) ? cardByCode(card.code.replace(/a$/, 'c')) : undefined)
  const isThirdFace = (card: Card) => /c$/.test(card.code) && byCode.value.has(card.code.replace(/c$/, 'a'))

  /** MarvelCDB's older single record of a double-sided card it also lists as linked A and B sides (Kang's 11007 beside 11007a/11007b). */
  const isSplitCard = (card: Card) => !/[a-z]$/.test(card.code) && !!cardByCode(`${card.code}a`)?.linkedTo

  /** What the browser lists: one entry a card, its other faces reached from it. */
  const cards = computed(() =>
    all.value.filter((c) => !c.duplicateOf && !isSplitCard(c) && (showHidden.value || (!c.hidden && !isThirdFace(c)))),
  )
  const results = computed(() =>
    sortCards(filterCards(cards.value, filters.value, rawByCode.value), sortField.value, sortDescending.value),
  )

  /** Facet values present in the database, with how many cards carry each. */
  const facets = computed(() => {
    const count = (values: (string | undefined)[]) => {
      const m = new Map<string, number>()
      for (const v of values) if (v) m.set(v, (m.get(v) ?? 0) + 1)
      return m
    }
    const sets = new Map<string, { name: string; count: number; type?: string }>()
    for (const c of cards.value) {
      if (!c.setCode || !c.setName) continue
      const seen = sets.get(c.setCode)
      sets.set(c.setCode, { name: c.setName, count: (seen?.count ?? 0) + 1, type: c.setType })
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

  return { all, cards, packs, state, error, filters, sortField, sortDescending, showHidden, load, cardByCode, cardBySet, errataFor, thirdFace, results, facets, hasFilters, filterCount, resetFilters, toggle }
})
