import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Support {
  supportId: string
  supportCardDef: CardDef
  supportUses: number
  supportExhausted: boolean
}

export const supportDecoder = JsonDecoder.object<Support>(
  {
    supportId: JsonDecoder.string,
    supportCardDef: cardDefDecoder,
    supportUses: JsonDecoder.number,
    supportExhausted: JsonDecoder.boolean,
  }, 'Support')
