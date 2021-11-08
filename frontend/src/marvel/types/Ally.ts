import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Ally {
  tag: string;
  contents: AllyContents
}

export interface AllyContents {
  allyId: string
  allyCardDef: CardDef
}

export const allyContentsDecoder = JsonDecoder.object<AllyContents>(
  {
    allyId: JsonDecoder.string,
    allyCardDef: cardDefDecoder,
  }, 'AllyContents')

export const allyDecoder = JsonDecoder.object<Ally>(
  {
    tag: JsonDecoder.string,
    contents: allyContentsDecoder,
  }, 'Ally')
