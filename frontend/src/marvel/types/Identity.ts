import { JsonDecoder } from 'ts.data.json'

export interface Identity {
  id: string
}

export const identityDecoder = JsonDecoder.object<Identity>({ id: JsonDecoder.string }, 'Identity')
