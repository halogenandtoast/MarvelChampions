import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface SideScheme {
  sideSchemeId: string
  sideSchemeCardDef: CardDef
  sideSchemeThreat: number
}

export const sideSchemeDecoder = JsonDecoder.object<SideScheme>(
  {
    sideSchemeId: JsonDecoder.string,
    sideSchemeCardDef: cardDefDecoder,
    sideSchemeThreat: JsonDecoder.number,
  }, 'SideScheme')
