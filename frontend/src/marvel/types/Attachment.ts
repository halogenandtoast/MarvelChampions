import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Attachment {
  attachmentId: string
  attachmentCardDef: CardDef
  attachmentDamage: number
}

export const attachmentDecoder = JsonDecoder.object<Attachment>(
  {
    attachmentId: JsonDecoder.string,
    attachmentCardDef: cardDefDecoder,
    attachmentDamage: JsonDecoder.number,
  }, 'Attachment')
