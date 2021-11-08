import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface VillainContents {
  villainId: string
  villainCardDef: CardDef
}

export interface Villain {
  tag: string
  contents: VillainContents
}

export const villainContentsDecoder = JsonDecoder.object<VillainContents>({
  villainId: JsonDecoder.string,
  villainCardDef: cardDefDecoder,
}, 'VillainContents')

export const villainDecoder = JsonDecoder.object<Villain>({
  tag: JsonDecoder.string,
  contents: villainContentsDecoder
}, 'Villain')
