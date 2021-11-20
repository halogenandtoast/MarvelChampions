import { JsonDecoder } from 'ts.data.json'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export interface Upgrade {
  tag: string;
  contents: UpgradeContents
}

export interface UpgradeContents {
  upgradeId: string
  upgradeCardDef: CardDef
  upgradeUses: number
  upgradeExhausted: boolean
}

export const upgradeContentsDecoder = JsonDecoder.object<UpgradeContents>(
  {
    upgradeId: JsonDecoder.string,
    upgradeCardDef: cardDefDecoder,
    upgradeUses: JsonDecoder.number,
    upgradeExhausted: JsonDecoder.boolean,
  }, 'UpgradeContents')

export const upgradeDecoder = JsonDecoder.object<Upgrade>(
  {
    tag: JsonDecoder.string,
    contents: upgradeContentsDecoder,
  }, 'Upgrade')
