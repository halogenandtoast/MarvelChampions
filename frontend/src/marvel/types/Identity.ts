import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'
import { PlayerCard, playerCardDecoder } from '@/marvel/types/PlayerCard'
import { EncounterCard, encounterCardDecoder } from '@/marvel/types/EncounterCard'

export interface HeroSideContentsContents {
  heroCardDef: CardDef
}

export interface AlterEgoSideContentsContents {
  alterEgoCardDef: CardDef
}

export interface HeroSideContents {
  contents: HeroSideContentsContents
}

export interface AlterEgoSideContents {
  contents: AlterEgoSideContentsContents
}

export interface HeroSide {
  tag: 'HeroSide'
  contents: HeroSideContents
}

export interface AlterEgoSide {
  tag: 'AlterEgoSide'
  contents: AlterEgoSideContents
}

type Side = HeroSide | AlterEgoSide

export interface Identity {
  id: string
  hand: PlayerCard[]
  discard: PlayerCard[]
  side: string
  sides: Record<string, Side>
  allies: string[]
  minions: string[]
  supports: string[]
  upgrades: string[]
  exhausted: boolean
  currentHP: number
  encounterCards: EncounterCard[]
}

export const heroSideContentsContentsDecoder = JsonDecoder.object<HeroSideContentsContents>({
  heroCardDef: cardDefDecoder
}, 'HeroSideContentsContents')

export const alterEgoSideContentsContentsDecoder = JsonDecoder.object<AlterEgoSideContentsContents>({
  alterEgoCardDef: cardDefDecoder
}, 'AlterEgoSideContentsContents')

export const heroSideContentsDecoder = JsonDecoder.object<HeroSideContents>({
  contents: heroSideContentsContentsDecoder
}, 'HeroSideContents')

export const alterEgoSideContentsDecoder = JsonDecoder.object<AlterEgoSideContents>({
  contents: alterEgoSideContentsContentsDecoder
}, 'AlterEgoSideContents')

export const heroSideDecoder = JsonDecoder.object<HeroSide>({
  tag: JsonDecoder.isExactly('HeroSide'),
  contents: heroSideContentsDecoder
}, 'HeroSide')

export const alterEgoSideDecoder = JsonDecoder.object<AlterEgoSide>({
  tag: JsonDecoder.isExactly('AlterEgoSide'),
  contents: alterEgoSideContentsDecoder
}, 'AlterEgoSide')

export const sideDecoder = JsonDecoder.oneOf<Side>([heroSideDecoder, alterEgoSideDecoder], 'Side')

export const identityDecoder = JsonDecoder.object<Identity>(
  {
    id: JsonDecoder.string,
    hand: JsonDecoder.array(playerCardDecoder, 'PlayerCard[]'),
    discard: JsonDecoder.array(playerCardDecoder, 'PlayerCard[]'),
    side: JsonDecoder.string,
    sides: JsonDecoder.array(JsonDecoder.
      tuple([JsonDecoder.string, sideDecoder], '[side,side]'), '[side,side][]').map(sides => Object.fromEntries(sides)),
    allies: JsonDecoder.array<string>(JsonDecoder.string, 'AllyId[]'),
    minions: JsonDecoder.array<string>(JsonDecoder.string, 'MinionId[]'),
    supports: JsonDecoder.array<string>(JsonDecoder.string, 'SupportId[]'),
    upgrades: JsonDecoder.array<string>(JsonDecoder.string, 'UpgradeId[]'),
    exhausted: JsonDecoder.boolean,
    currentHP: JsonDecoder.number,
    encounterCards: JsonDecoder.array(encounterCardDecoder, 'EncounterCard[]'),
  }, 'Identity')
