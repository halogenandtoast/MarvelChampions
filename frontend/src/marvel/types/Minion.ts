import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Minion {
  tag: string;
  contents: MinionContents
}

export interface MinionContents {
  minionId: string
  minionCardDef: CardDef
  minionDamage: number
  minionUpgrades: string[]
}

export const minionContentsDecoder = JsonDecoder.object<MinionContents>(
  {
    minionId: JsonDecoder.string,
    minionCardDef: cardDefDecoder,
    minionDamage: JsonDecoder.number,
    minionUpgrades: JsonDecoder.array<string>(JsonDecoder.string, 'UpgradeId[]'),
  }, 'MinionContents')

export const minionDecoder = JsonDecoder.object<Minion>(
  {
    tag: JsonDecoder.string,
    contents: minionContentsDecoder,
  }, 'Minion')
