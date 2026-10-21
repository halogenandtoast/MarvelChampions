/**
 * Decks, after app/Decks.hs. Keep the two in step: the Haskell side is authoritative.
 */
import type { Card } from './card'

/** One line of a deck list; `card` is null for a code the card database does not have. */
export interface Slot {
  quantity: number
  code: string
  card: Card | null
}

export interface HeroSummary {
  code: string
  name: string
}

export interface Deck {
  id: number
  name: string
  description: string
  hero: HeroSummary
  aspects: string[]
  /** `marvelcdb` for a deck fetched by link, `json` for an uploaded file. */
  source: string
  sourceId: string | null
  sourceUrl: string | null
  createdAt: string
  updatedAt: string
  slots: Slot[]
  /** Why the engine cannot deal it, if it cannot: an i18n key. */
  problem: string | null
}

/** A deck without its cards: what the deck list shows. */
export interface DeckSummary {
  id: number
  name: string
  hero: HeroSummary
  aspects: string[]
  source: string
  sourceUrl: string | null
  cardCount: number
  problem: string | null
  updatedAt: string
  /** When a game was last dealt from it. */
  usedAt: string | null
}

/** Exactly one of `url` and `deck`; `name` overrides the name in the source. */
export interface ImportRequest {
  url?: string
  deck?: unknown
  name?: string
  /** False to load the deck without keeping it, for the game at hand. */
  save?: boolean
}

/** A deck as the engine deals it. */
export interface DeckList {
  hero: string
  aspects: string[]
  cards: [string, number][]
}

/** A deck loaded but not kept: what it holds, and its list, or why it cannot be played. */
export interface DeckPreview {
  name: string
  hero: HeroSummary
  aspects: string[]
  slots: Slot[]
  cardCount: number
  list: DeckList | null
  problem: string | null
}
