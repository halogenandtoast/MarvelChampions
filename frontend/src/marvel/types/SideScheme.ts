import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface SideScheme {
  tag: string;
  contents: SideSchemeContents
}

export interface SideSchemeContents {
  sideSchemeId: string
  sideSchemeCardDef: CardDef
  sideSchemeThreat: number
}

export const sideSchemeContentsDecoder = JsonDecoder.object<SideSchemeContents>(
  {
    sideSchemeId: JsonDecoder.string,
    sideSchemeCardDef: cardDefDecoder,
    sideSchemeThreat: JsonDecoder.number,
  }, 'SideSchemeContents')

export const sideSchemeDecoder = JsonDecoder.object<SideScheme>(
  {
    tag: JsonDecoder.string,
    contents: sideSchemeContentsDecoder,
  }, 'SideScheme')
