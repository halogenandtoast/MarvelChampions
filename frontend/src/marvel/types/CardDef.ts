import { JsonDecoder } from 'ts.data.json'

export interface CardDef {
  cdCardCode: string
}

export const cardDefDecoder = JsonDecoder.object<CardDef>({
  cdCardCode: JsonDecoder.string
}, 'CardDef')
