/**
 * The card database, as frontend/public/data/cards.json has it: MarvelCDB's cards, with
 * the scan each is shown by.
 */

export type Orientation = 'portrait' | 'landscape'

export type Faction = 'aggression' | 'justice' | 'leadership' | 'protection' | 'basic' | 'hero' | 'encounter' | 'campaign' | 'pool'

export type CardType =
  | 'hero' | 'alter_ego' | 'ally' | 'event' | 'support' | 'upgrade' | 'resource' | 'player_side_scheme'
  | 'villain' | 'main_scheme' | 'side_scheme' | 'minion' | 'treachery' | 'attachment' | 'obligation' | 'environment'
  | 'leader' | 'evidence_means' | 'evidence_motive' | 'evidence_opportunity'

export type Resource = 'energy' | 'mental' | 'physical' | 'wild'

export interface Card {
  code: string
  name: string
  /** Earlier local import codes that still resolve to this printing. */
  aliases?: string[]
  subname?: string
  packCode: string
  packName: string
  packOrder?: number
  position?: number
  setCode?: string
  setName?: string
  setType?: string
  setPosition?: number
  type: CardType | string
  typeName?: string
  faction?: Faction | string
  factionName?: string
  cost?: number | null
  resources?: Partial<Record<Resource, number>>
  thwart?: number | null
  attack?: number | null
  defense?: number | null
  recover?: number | null
  scheme?: number | null
  health?: number | null
  healthPerHero?: boolean
  handSize?: number | null
  boost?: number | null
  attackStar?: boolean
  schemeStar?: boolean
  boostStar?: boolean
  threat?: number | null
  threatFixed?: boolean
  baseThreatFixed?: boolean
  acceleration?: number | null
  crisis?: number | null
  hazard?: number | null
  amplify?: number | null
  textVerified?: boolean
  baseThreat?: number | null
  escalationThreat?: number | null
  escalationThreatFixed?: boolean
  stage?: string | null
  traits?: string[]
  text?: string
  backText?: string
  backName?: string
  flavor?: string
  backFlavor?: string
  illustrator?: string
  quantity?: number
  deckLimit?: number
  unique?: boolean
  permanent?: boolean
  hidden?: boolean
  doubleSided?: boolean
  linkedTo?: string
  duplicateOf?: string
  image?: string
  /** Custom imports ship with the app instead of the external scan bucket. */
  bundledImage?: boolean
  remoteImage?: string
  remoteBackImage?: string
  orientation?: Orientation
  /** Imported scans may have a source outside MarvelCDB and unverified OCR. */
  sourceUrl?: string
  ocr?: { engine: string; verified: boolean; rawText: string }
  /** Nonstandard printed dimensions, as for an unfolded identity. */
  aspectRatio?: number
}

export interface Pack {
  code: string
  name: string
  order: number
  released?: string
  cardCount: number
}

export interface CardIndex {
  version: number
  generatedAt: string
  packs: Pack[]
  cards: Card[]
}

export const ASPECTS = ['aggression', 'justice', 'leadership', 'protection', 'pool'] as const
export const FACTIONS = ['aggression', 'justice', 'leadership', 'protection', 'basic', 'hero', 'encounter', 'campaign', 'pool'] as const

/** Types a player builds with, in the order a deck list is written. */
export const PLAYER_TYPES = ['hero', 'alter_ego', 'ally', 'event', 'support', 'upgrade', 'resource', 'player_side_scheme'] as const
export const ENCOUNTER_TYPES = ['villain', 'main_scheme', 'side_scheme', 'minion', 'treachery', 'attachment', 'obligation', 'environment', 'leader'] as const

/** Where the card scans are served from: this site, or a bucket named at build time. */
export const CARD_IMAGES = `${import.meta.env.VITE_ASSET_HOST ?? ''}/images/cards`

/** Where a card's scan is kept, by its file in the HD export. */
export const localImage = (card: Pick<Card, 'code' | 'image' | 'bundledImage'>) => `${card.bundledImage ? '/images/cards' : CARD_IMAGES}/${card.image ?? `${card.code}.avif`}`

/** MarvelCDB's own scan, for a card the export has none of. */
export const remoteImage = (card: Pick<Card, 'remoteImage'>) => (card.remoteImage ? `https://marvelcdb.com${card.remoteImage}` : null)
