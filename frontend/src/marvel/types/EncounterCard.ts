import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface EncounterCard {
  cardId: string
  cardDef: CardDef
}

export const encounterCardDecoder = JsonDecoder.object<EncounterCard>({
  cardId: JsonDecoder.string,
  cardDef: cardDefDecoder
}, 'EncounterCard', { cardId: 'ecCardId', cardDef: 'ecCardDef' })

