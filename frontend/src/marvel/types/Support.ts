import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Support {
  tag: string;
  contents: SupportContents
}

export interface SupportContents {
  supportId: string
  supportCardDef: CardDef
  supportExhausted: boolean
}

export const supportContentsDecoder = JsonDecoder.object<SupportContents>(
  {
    supportId: JsonDecoder.string,
    supportCardDef: cardDefDecoder,
    supportExhausted: JsonDecoder.boolean,
  }, 'SupportContents')

export const supportDecoder = JsonDecoder.object<Support>(
  {
    tag: JsonDecoder.string,
    contents: supportContentsDecoder,
  }, 'Support')
