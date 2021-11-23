import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface VillainContents {
  villainId: string
  villainCardDef: CardDef
  villainHp: number
  villainMaxHp: number
  villainStunned: boolean
  villainConfused: boolean
  villainTough: boolean
  villainAttachments: string[]
  villainUpgrades: string[]
}

export interface Villain {
  tag: string
  contents: VillainContents
}

export const villainContentsDecoder = JsonDecoder.object<VillainContents>({
  villainId: JsonDecoder.string,
  villainCardDef: cardDefDecoder,
  villainHp: JsonDecoder.number,
  villainMaxHp: JsonDecoder.number,
  villainStunned: JsonDecoder.boolean,
  villainConfused: JsonDecoder.boolean,
  villainTough: JsonDecoder.boolean,
  villainAttachments: JsonDecoder.array<string>(JsonDecoder.string, 'AttachmentId[]'),
  villainUpgrades: JsonDecoder.array<string>(JsonDecoder.string, 'UpgradeId[]'),
}, 'VillainContents')

export const villainDecoder = JsonDecoder.object<Villain>({
  tag: JsonDecoder.string,
  contents: villainContentsDecoder
}, 'Villain')
