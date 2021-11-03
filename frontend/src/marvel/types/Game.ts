import { JsonDecoder } from 'ts.data.json'
import { Identity, identityDecoder } from '@/marvel/types/Identity'

export interface Game {
  id: string
  name: string
  players: Record<string, Identity>
  scenario: Scenario
}

export interface Scenario {
  contents: ScenarioContents
}

export interface ScenarioContents {
  scenarioId: string
}

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>(
  {
    scenarioId: JsonDecoder.string
  }, 'ScenarioContents')

export const scenarioDecoder = JsonDecoder.object<Scenario>(
  {
    contents: scenarioContentsDecoder
  }, 'Scenario')

export const gameDecoder = JsonDecoder.object<Game>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    players: JsonDecoder.dictionary<Identity>(identityDecoder, 'Dict<UUID, Identity>'),
    scenario: scenarioDecoder,
  }, 'Game')
