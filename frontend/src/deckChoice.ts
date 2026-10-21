import type { DeckChoice } from './api'
import type { DeckSummary } from './browser/types/deck'
import type { StarterDeck } from './server'

/** A deck as a string, for v-model: `starter:<hero>`, `deck:<id>`, or `loaded` for one loaded for this game. */
export type DeckKey = string

export const toDeckChoice = (key: DeckKey): DeckChoice => {
  const [kind, rest] = key.split(':')
  return kind === 'deck' ? { deckId: Number(rest) } : { starter: rest }
}

export interface DeckFace {
  key: DeckKey
  starter: boolean
  /** the identity's code, either face */
  hero: string
  /** a deck's own name; a starter is named by its hero */
  name: string | null
  aspects: string[]
  problem: string | null
}

export const deckFaces = (starters: StarterDeck[], decks: DeckSummary[]): DeckFace[] => [
  ...starters.map((s) => ({ key: `starter:${s.key}`, starter: true, hero: s.hero, name: null, aspects: s.aspects, problem: null })),
  ...decks.map((d) => ({ key: `deck:${d.id}`, starter: false, hero: d.hero.code, name: d.name, aspects: d.aspects, problem: d.problem })),
]

/** The hero face of an identity, for its portrait: `01001b` shows `01001a`. */
export const heroFace = (code: string) => code.replace(/b$/, 'a')
