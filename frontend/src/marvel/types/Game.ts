import { JsonDecoder } from 'ts.data.json';

export interface Game {
  id: string;
  name: string;
}

export const gameDecoder = JsonDecoder.object<Game>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
  }, 'Game')
