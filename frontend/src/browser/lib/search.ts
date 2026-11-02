/**
 * Card searching and filtering, in the browser: the whole database is a few thousand
 * cards, so there is no round trip between keystrokes.
 */
import type { Card } from '@browser/types/card'

/** Fold case, strip accents and drop punctuation, so a plain keyboard finds `Doctor Strange`. */
export function normalize(text: string): string {
  return text
    .normalize('NFKD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '')
}

export interface CardFilters {
  search: string
  types: string[]
  factions: string[]
  packs: string[]
  sets: string[]
  traits: string[]
  uniqueOnly: boolean
  costMin: number | null
  costMax: number | null
}

export function emptyFilters(): CardFilters {
  return { search: '', types: [], factions: [], packs: [], sets: [], traits: [], uniqueOnly: false, costMin: null, costMax: null }
}

/** How many facets are active, for the badge on the Filters button. */
export function activeFilterCount(f: CardFilters): number {
  return (
    f.types.length +
    f.factions.length +
    f.packs.length +
    f.sets.length +
    f.traits.length +
    (f.uniqueOnly ? 1 : 0) +
    (f.costMin !== null || f.costMax !== null ? 1 : 0)
  )
}

export function isEmptyFilters(f: CardFilters): boolean {
  return f.search.trim() === '' && activeFilterCount(f) === 0
}

// a card's searchable text, normalised once and cached on the card
const haystacks = new WeakMap<Card, string>()
function haystack(card: Card): string {
  let value = haystacks.get(card)
  if (value === undefined) {
    value = normalize([card.name, card.subname ?? '', ...(card.traits ?? []), card.text ?? '', card.backText ?? '', card.setName ?? '', card.packName].join(' '))
    haystacks.set(card, value)
  }
  return value
}

export function searchTerms(query: string): string[] {
  return query.split(/\s+/).map(normalize).filter((t) => t.length > 0)
}

export function filterCards(cards: Card[], filters: CardFilters): Card[] {
  const terms = searchTerms(filters.search)
  const traits = filters.traits.map(normalize)
  const bounded = filters.costMin !== null || filters.costMax !== null
  return cards.filter((card) => {
    if (terms.length && !terms.every((term) => haystack(card).includes(term))) return false
    if (filters.types.length && !filters.types.includes(card.type)) return false
    if (filters.factions.length && !filters.factions.includes(card.faction ?? '')) return false
    if (filters.packs.length && !filters.packs.includes(card.packCode)) return false
    if (filters.sets.length && !filters.sets.includes(card.setCode ?? '')) return false
    if (filters.uniqueOnly && !card.unique) return false
    // conjunctive: an Avenger Hero for Hire filter wants both
    if (traits.length) {
      const own = (card.traits ?? []).map(normalize)
      if (!traits.every((t) => own.includes(t))) return false
    }
    if (bounded) {
      const cost = typeof card.cost === 'number' ? card.cost : null
      if (cost === null) return false
      if (filters.costMin !== null && cost < filters.costMin) return false
      if (filters.costMax !== null && cost > filters.costMax) return false
    }
    return true
  })
}

export type SortField = 'set' | 'name' | 'cost' | 'type' | 'faction'
export const SORT_FIELDS: SortField[] = ['set', 'name', 'cost', 'type', 'faction']

export function sortCards(cards: Card[], field: SortField, descending = false): Card[] {
  if (field === 'set' && !descending) return cards
  const byName = (a: Card, b: Card) => normalize(a.name).localeCompare(normalize(b.name))
  // cards with no value for the key go last, whichever way
  const last = (v: unknown) => (v === null || v === undefined ? 1 : 0)
  const sorted = [...cards].sort((a, b) => {
    switch (field) {
      case 'set':
        return 0
      case 'name':
        return byName(a, b)
      case 'cost':
        return last(a.cost) - last(b.cost) || (a.cost ?? 0) - (b.cost ?? 0) || byName(a, b)
      case 'type':
        return a.type.localeCompare(b.type) || byName(a, b)
      case 'faction':
        return (a.faction ?? '').localeCompare(b.faction ?? '') || byName(a, b)
    }
  })
  return descending ? sorted.reverse() : sorted
}
