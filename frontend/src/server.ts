// The shapes the server sends, after Marvel.Game.Public and app/Main.hs. The game view
// grows as the engine does, so every record is open and the board reads it defensively.

import type { LogEntry } from './gameLog'

export interface CardView {
  id: string
  code: string
  name: string
  [more: string]: unknown
}

export interface UpgradeView extends CardView {
  exhausted?: boolean
  uses?: number
  controller?: string
}

export interface SupportView extends CardView {
  exhausted?: boolean
  uses?: number
}

export interface AttachmentView extends CardView {
  damage?: number
}

export interface AllyView extends CardView {
  hitPoints: number
  damage: number
  thwart: number
  attack: number
  exhausted: boolean
  stunned: boolean
  confused: boolean
  tough: boolean
  counters?: number
  upgrades?: UpgradeView[]
}

export interface PlayerView {
  id: string
  name: string
  form: 'hero' | 'alterEgo' | string
  code: string
  otherCode: string
  hitPoints: number
  damage: number
  exhausted: boolean
  stunned: boolean
  confused: boolean
  tough: boolean
  thwart: number | null
  attack: number | null
  defense: number | null
  recover: number | null
  handSize: number
  handCount: number
  hand: CardView[] | null
  deckSize: number
  discard: CardView[]
  dealtEncounterCards: number
  allies: AllyView[]
  supports: SupportView[]
  upgrades: UpgradeView[]
  engagedMinions: string[]
  [more: string]: unknown
}

export interface VillainView extends CardView {
  stage: number
  hitPoints: number
  damage: number
  scheme: number
  attack: number
  stunned: boolean
  confused: boolean
  tough: boolean
  boostCards: number
  attachments: AttachmentView[]
  upgrades: UpgradeView[]
}

export interface MinionView extends CardView {
  engagedWith: string
  hitPoints: number
  damage: number
  scheme: number
  attack: number
  stunned: boolean
  confused: boolean
  tough: boolean
  attachments: AttachmentView[]
  upgrades: UpgradeView[]
}

export interface SchemeView extends CardView {
  threat: number
  crisis: boolean
}

export type ChoiceKind =
  | 'label' | 'target' | 'endTurn' | 'ability' | 'changeForm' | 'play' | 'pay' | 'finishPayment'
  | 'recover' | 'attack' | 'thwart' | 'defend' | 'allyAttack' | 'allyThwart' | 'allyDefend'
  | 'damage' | 'thwartScheme' | 'stun' | 'confuse' | 'heal' | 'discard' | 'returnToHand'
  | 'player' | 'other'

export interface ChoiceView {
  index: number
  kind: ChoiceKind | string
  /** An i18n key when it starts with `$`, plain text otherwise. */
  label: string | null
  /** The id of the card or entity the choice is about. */
  target: string | null
  code: string | null
  abilityType: string | null
}

export interface QuestionView {
  kind: 'chooseOne' | 'chooseN' | 'chooseOneAtATime' | 'choosePlayerOrder' | string
  choices: ChoiceView[]
}

export interface GameView {
  round: number
  phase: 'PlayerPhase' | 'VillainPhase' | string
  outcome: 'Won' | 'Lost' | null
  activePlayer: string
  playerOrder: string[]
  seats: string[]
  players: PlayerView[]
  villains: VillainView[]
  mainSchemes: SchemeView[]
  sideSchemes: SchemeView[]
  minions: MinionView[]
  encounterDeckSize: number
  encounterDiscard: CardView[]
  focusedCards: CardView[]
  accelerationTokens: number
  question: Record<string, QuestionView>
  questionVersion: number
  [more: string]: unknown
}

/** Who plays: with friends, who sits where (seat n plays the nth player); otherwise the owner plays every seat. */
export interface Table {
  withFriends: boolean
  seats: number
  owner: number
  seated: { position: number; userId: number; username: string }[]
}

export interface GameData {
  started: boolean
  game: GameView | null
  playerId: string | null
  canUndo: boolean
  table: Table
  /** The log's latest entries, when the client should take them afresh. */
  log: LogEntry[] | null
}

export type ServerMessage =
  | { tag: 'Update'; contents: GameData }
  | { tag: 'Logged'; entry: LogEntry }
  | { tag: 'ShuffleIntoDeck'; cardIds: string[] }

export interface SeatSummary {
  name: string
  identity: string
  damage: number
  eliminated: boolean
}

export interface GameSummary {
  id: string
  name: string
  playerName: string
  /** The villain's name. */
  scenario: string
  villain: string
  difficulty: 'Normal' | 'Expert' | string
  round: number
  phase: string
  outcome: 'Won' | 'Lost' | null
  /** Waiting for friends to take the rest of the seats. */
  waiting: boolean
  /** Who plays, in turn order, as whom; none until the game is dealt. */
  seats: SeatSummary[]
}

export interface ScenarioOption {
  code: string
  name: string
  modulars: string[]
}

export interface StarterDeck {
  key: string
  hero: string
  aspects: string[]
}

/** The modular encounter sets of the core set, by the engine's names. */
export const MODULAR_SETS = ['BombScare', 'MastersOfEvil', 'UnderAttack', 'LegionsOfHydra', 'TheDoomsdayChair'] as const
