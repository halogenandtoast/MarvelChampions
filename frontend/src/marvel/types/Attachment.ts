import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Attachment {
  tag: string;
  contents: AttachmentContents
}

export interface AttachmentContents {
  attachmentId: string
  attachmentCardDef: CardDef
}

export const attachmentContentsDecoder = JsonDecoder.object<AttachmentContents>(
  {
    attachmentId: JsonDecoder.string,
    attachmentCardDef: cardDefDecoder,
  }, 'AttachmentContents')

export const attachmentDecoder = JsonDecoder.object<Attachment>(
  {
    tag: JsonDecoder.string,
    contents: attachmentContentsDecoder,
  }, 'Attachment')
