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
  /** the cards attached faceup to it, played from there (Hawkeye's Quiver) */
  heldCards?: CardView[]
}

export interface SupportView extends CardView {
  exhausted?: boolean
  uses?: number
  threat?: number
  /** cards attached to it facedown */
  facedownCards?: number
}

export interface AttachmentView extends CardView {
  damage?: number
  counters?: number
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
  allyLimit: number
  supports: SupportView[]
  upgrades: UpgradeView[]
  engagedMinions: string[]
  /** encounter cards attached to their hero */
  attachments?: AttachmentView[]
  /** the obligations in their play area */
  obligations?: AttachmentView[]
  /** Doctor Strange's Invocation deck: its top card is faceup */
  invocation?: { top: CardView | null; deckSize: number; discard: CardView[] } | null
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
  /** the active villain of his area: the one that activates */
  active?: boolean
}

/** A deck other than the encounter deck: a villain's own, a named one, or the side-scheme deck. */
export interface DeckView {
  kind: 'villain' | 'named' | 'sideScheme' | string
  /** the villain's name, or the named deck's */
  name: string | null
  size: number
  discard: CardView[]
}

/** A game area: what is in it, by id. A game of one area has one. */
export interface AreaView {
  id: number
  players: string[]
  villains: string[]
  activeVillain: string | null
  mainSchemes: string[]
  sideSchemes: string[]
  environments: string[]
  decks: DeckView[]
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
  boostCards: number
  attachments: AttachmentView[]
  upgrades: UpgradeView[]
}

export interface SchemeView extends CardView {
  threat: number
  crisis: boolean
  upgrades?: UpgradeView[]
  counters?: number
  attachments?: AttachmentView[]
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
  /** What is asked: an i18n key when it starts with `$`, plain text otherwise. */
  prompt?: string | null
  /** The id of the card or entity the question is about. */
  source?: string | null
  choices: ChoiceView[]
}

/** A card or ability being paid for, and the resources paid so far. */
export interface ActiveCostView {
  player: string
  /** the card being played */
  card: CardView | null
  /** or the card whose ability is being paid for, by id */
  source: string | null
  paid: string[]
  /** what the paid resources do not yet cover: a resource of that kind, or null for any */
  owed: (string | null)[]
  /** an X cost, paid as far as the player likes */
  anyAmount: boolean
  spent: CardView[]
}

export interface EnvironmentView {
  id: string
  code: string
  name: string
  attachments?: { id: string; code: string; name: string; damage?: number; counters?: number }[]
  counters?: number
}

export interface BoostView {
  id: string
  code: string | null
  name: string | null
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
  /** Environments: lasting encounter cards, with what is attached to them. */
  environments?: EnvironmentView[]
  encounterDeckSize: number
  encounterDiscard: CardView[]
  focusedCards: CardView[]
  /** an enemy's attack, while it is made: ids of who attacks and who is attacked */
  attack?: { attacker: string; target: string } | null
  /** boost cards: face down, without a code, until flipped */
  boostCards?: BoostView[]
  accelerationTokens: number
  question: Record<string, QuestionView>
  questionVersion: number
  activeCost?: ActiveCostView | null
  areas?: AreaView[]
  /** the area of the first seat whose hand you see */
  viewerArea?: number | null
  [more: string]: unknown
}

/** Who plays: with friends, who sits where (seat n plays the nth player); otherwise the owner plays every seat. */
export interface Table {
  withFriends: boolean
  seats: number
  owner: number
  seated: { position: number; userId: number; username: string; hero?: string | null }[]
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
  | { tag: 'Shuffled'; pile: string }
  | { tag: 'BoostDealt'; cardId: string }
  | { tag: 'Revealing'; cardId: string; code: string; boost: boolean }

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
  /** the villain's card code */
  code: string
  name: string
  /** the modular sets it recommends, by the engine's names */
  modulars: string[]
}

/** What can be played: the scenarios, and the modular sets any of them may take. */
export interface ScenarioList {
  scenarios: ScenarioOption[]
  modulars: string[]
}

export interface StarterDeck {
  key: string
  hero: string
  aspects: string[]
}
