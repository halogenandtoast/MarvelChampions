import { JsonDecoder } from 'ts.data.json'
import { Identity, identityDecoder } from '@/marvel/types/Identity'
import { Villain, villainDecoder } from '@/marvel/types/Villain'
import { Ally, allyDecoder } from '@/marvel/types/Ally'
import { Minion, minionDecoder } from '@/marvel/types/Minion'
import { Support, supportDecoder } from '@/marvel/types/Support'
import { Attachment, attachmentDecoder } from '@/marvel/types/Attachment'
import { Choice, Question, questionDecoder } from '@/marvel/types/Question'
import { CardDef, cardDefDecoder } from '@/marvel/types/CardDef'

export function choices(game: Game, identityId: string): Choice[] {
  const question = game.question[identityId]

  if (!question) {
    return []
  }

  switch (question.tag) {
    case 'ChooseOne':
      return question.contents
    default:
      return []
  }
}

export interface Game {
  id: string
  name: string
  players: Record<string, Identity>
  villains: Record<string, Villain>
  allies: Record<string, Ally>
  minions: Record<string, Minion>
  supports: Record<string, Support>
  attachments: Record<string, Attachment>
  scenario: Scenario
  question: Record<string, Question>
}

export interface Scenario {
  contents: ScenarioContents
}

export interface EncounterCard {
  cardId: string
  cardDef: CardDef
}

export const encounterCardDecoder = JsonDecoder.object<EncounterCard>({
  cardId: JsonDecoder.string,
  cardDef: cardDefDecoder
}, 'EncounterCard', { cardId: 'ecCardId', cardDef: 'ecCardDef' })

export interface ScenarioContents {
  scenarioId: string
  scenarioThreat: number
  scenarioDiscard: EncounterCard[]
}

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>(
  {
    scenarioId: JsonDecoder.string,
    scenarioThreat: JsonDecoder.number,
    scenarioDiscard: JsonDecoder.array(encounterCardDecoder, 'EncounterCard[]'),
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
    villains: JsonDecoder.dictionary<Villain>(villainDecoder, 'Dict<UUID, Villain>'),
    allies: JsonDecoder.dictionary<Ally>(allyDecoder, 'Dict<UUID, Ally>'),
    minions: JsonDecoder.dictionary<Minion>(minionDecoder, 'Dict<UUID, Minion>'),
    supports: JsonDecoder.dictionary<Support>(supportDecoder, 'Dict<UUID, Support>'),
    attachments: JsonDecoder.dictionary<Attachment>(attachmentDecoder, 'Dict<UUID, Attachment>'),
    scenario: scenarioDecoder,
    question: JsonDecoder.dictionary<Question>(questionDecoder, 'Dict<UUID, Question'),
  }, 'Game')
