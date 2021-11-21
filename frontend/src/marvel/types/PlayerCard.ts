import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface PlayerCard {
  cardId: string
  cardDef: CardDef
}

export const playerCardDecoder = JsonDecoder.object<PlayerCard>({
  cardId: JsonDecoder.string,
  cardDef: cardDefDecoder
}, 'PlayerCard', { cardId: 'pcCardId', cardDef: 'pcCardDef' })

