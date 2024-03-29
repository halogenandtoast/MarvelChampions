import { JsonDecoder } from 'ts.data.json'
import { Identity, identityDecoder } from '@/marvel/types/Identity'
import { Villain, villainDecoder } from '@/marvel/types/Villain'
import { Ally, allyDecoder } from '@/marvel/types/Ally'
import { Minion, minionDecoder } from '@/marvel/types/Minion'
import { Support, supportDecoder } from '@/marvel/types/Support'
import { Upgrade, upgradeDecoder } from '@/marvel/types/Upgrade'
import { Attachment, attachmentDecoder } from '@/marvel/types/Attachment'
import { PlayerCard, playerCardDecoder } from '@/marvel/types/PlayerCard'
import { SideScheme, sideSchemeDecoder } from '@/marvel/types/SideScheme'
import { MainScheme, mainSchemeDecoder } from '@/marvel/types/MainScheme'
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
    case 'ChooseOneAtATime':
      return question.contents
    default:
      return []
  }
}

export type State = Unstarted | InProgress | Finished

export interface Unstarted {
  tag: 'Unstarted'
}

export const unstartedDecoder = JsonDecoder.object<Unstarted>({ tag: JsonDecoder.isExactly('Unstarted') }, 'Unstarted')

export interface InProgress {
  tag: 'InProgress'
}

export const inProgressDecoder = JsonDecoder.object<InProgress>({ tag: JsonDecoder.isExactly('InProgress') }, 'InProgress')

export interface Finished {
  tag: 'Finished'
  contents: FinishedContents
}

export const finishedContentsDecoder = JsonDecoder.oneOf<FinishedContents>(
  [ JsonDecoder.isExactly('Won')
  , JsonDecoder.isExactly('Lost')
  ]
, 'FinishedContents')


export const finishedDecoder = JsonDecoder.object<Finished>({
  tag: JsonDecoder.isExactly('Finished'),
  contents: finishedContentsDecoder
}, 'Finished')

type FinishedContents = 'Won' | 'Lost'

export const stateDecoder = JsonDecoder.oneOf<State>([ unstartedDecoder, inProgressDecoder, finishedDecoder ], 'State')

export interface Game {
  id: string
  name: string
  players: Record<string, Identity>
  villains: Record<string, Villain>
  allies: Record<string, Ally>
  minions: Record<string, Minion>
  supports: Record<string, Support>
  upgrades: Record<string, Upgrade>
  attachments: Record<string, Attachment>
  sideSchemes: Record<string, SideScheme>
  mainSchemes: Record<string, MainScheme>
  scenario: Scenario
  question: Record<string, Question>
  state: State
  focusedCards: Card[]
}

export interface CardPlayerCard {
  tag: 'PlayerCard'
  contents: PlayerCard
}

export interface CardEncounterCard {
  tag: 'EncounterCard'
  contents: EncounterCard
}

export type Card = CardPlayerCard | CardEncounterCard

export interface EncounterCard {
  cardId: string
  cardDef: CardDef
}

export const encounterCardDecoder = JsonDecoder.object<EncounterCard>({
  cardId: JsonDecoder.string,
  cardDef: cardDefDecoder
}, 'EncounterCard', { cardId: 'ecCardId', cardDef: 'ecCardDef' })

export const cardDecoder = JsonDecoder.oneOf<Card>([
  JsonDecoder.object<CardPlayerCard>(
    {
      tag: JsonDecoder.isExactly('PlayerCard'),
      contents: playerCardDecoder
    }, 'CardPlayerCard'),
  JsonDecoder.object<CardEncounterCard>(
    {
      tag: JsonDecoder.isExactly('EncounterCard'),
      contents: encounterCardDecoder
    }, 'CardEncounterCard')
  ], 'Card')

export interface Scenario {
  scenarioId: string
  scenarioAccelerationTokens: number
  scenarioDiscard: EncounterCard[]
}

export const scenarioDecoder = JsonDecoder.object<Scenario>(
  {
    scenarioId: JsonDecoder.string,
    scenarioAccelerationTokens: JsonDecoder.number,
    scenarioDiscard: JsonDecoder.array(encounterCardDecoder, 'EncounterCard[]'),
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
    upgrades: JsonDecoder.dictionary<Upgrade>(upgradeDecoder, 'Dict<UUID, Upgrade>'),
    attachments: JsonDecoder.dictionary<Attachment>(attachmentDecoder, 'Dict<UUID, Attachment>'),
    sideSchemes: JsonDecoder.dictionary<SideScheme>(sideSchemeDecoder, 'Dict<UUID, SideScheme>'),
    mainSchemes: JsonDecoder.dictionary<MainScheme>(mainSchemeDecoder, 'Dict<UUID, MainScheme>'),
    scenario: scenarioDecoder,
    question: JsonDecoder.dictionary<Question>(questionDecoder, 'Dict<UUID, Question'),
    state: stateDecoder,
    focusedCards: JsonDecoder.array<Card>(cardDecoder, 'Card[]')
  }, 'Game')
