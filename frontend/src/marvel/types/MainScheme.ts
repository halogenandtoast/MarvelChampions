import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface MainScheme {
  tag: string;
  contents: MainSchemeContents
}

export interface MainSchemeContents {
  mainSchemeId: string
  mainSchemeCardDef: CardDef
  mainSchemeThreat: number
}

export const mainSchemeContentsDecoder = JsonDecoder.object<MainSchemeContents>(
  {
    mainSchemeId: JsonDecoder.string,
    mainSchemeCardDef: cardDefDecoder,
    mainSchemeThreat: JsonDecoder.number,
  }, 'MainSchemeContents')

export const mainSchemeDecoder = JsonDecoder.object<MainScheme>(
  {
    tag: JsonDecoder.string,
    contents: mainSchemeContentsDecoder,
  }, 'MainScheme')
