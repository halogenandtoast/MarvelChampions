import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Upgrade {
  upgradeId: string
  upgradeCardDef: CardDef
  upgradeUses: number
  upgradeExhausted: boolean
}

export const upgradeDecoder = JsonDecoder.object<Upgrade>(
  {
    upgradeId: JsonDecoder.string,
    upgradeCardDef: cardDefDecoder,
    upgradeUses: JsonDecoder.number,
    upgradeExhausted: JsonDecoder.boolean,
  }, 'Upgrade')
