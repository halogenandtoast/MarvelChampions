import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface MainScheme {
  mainSchemeId: string
  mainSchemeCardDef: CardDef
  mainSchemeThreat: number
}

export const mainSchemeDecoder = JsonDecoder.object<MainScheme>(
  {
    mainSchemeId: JsonDecoder.string,
    mainSchemeCardDef: cardDefDecoder,
    mainSchemeThreat: JsonDecoder.number,
  }, 'MainScheme')
