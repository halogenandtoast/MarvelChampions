import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Ally {
  tag: string;
  contents: AllyContents
}

export interface AllyContents {
  allyId: string
  allyCardDef: CardDef
  allyDamage: number
  allyCounters: number
  allyExhausted: boolean
  allyTough: boolean
  allyConfused: boolean
  allyStunned: boolean
  allyUpgrades: string[]
}

export const allyContentsDecoder = JsonDecoder.object<AllyContents>(
  {
    allyId: JsonDecoder.string,
    allyCardDef: cardDefDecoder,
    allyDamage: JsonDecoder.number,
    allyCounters: JsonDecoder.number,
    allyExhausted: JsonDecoder.boolean,
    allyTough: JsonDecoder.boolean,
    allyConfused: JsonDecoder.boolean,
    allyStunned: JsonDecoder.boolean,
    allyUpgrades: JsonDecoder.array<string>(JsonDecoder.string, 'UpgradeId[]'),
  }, 'AllyContents')

export const allyDecoder = JsonDecoder.object<Ally>(
  {
    tag: JsonDecoder.string,
    contents: allyContentsDecoder,
  }, 'Ally')
