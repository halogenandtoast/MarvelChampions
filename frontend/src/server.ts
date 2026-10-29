// The shapes the server sends, after Marvel.Game.Public and app/Main.hs. The game view
// grows as the engine does, so every record is open and the board reads it defensively.

import type { LogEntry } from './gameLog'

/** Cards searched, and where they were (an i18n key) when it was one of several places. */
export interface SearchedView {
  place: string | null
  cards: CardView[]
}

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
  /** a Spectrum energy form she is not in */
  facedown?: boolean
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
  facedownCards?: number
  upgrades?: UpgradeView[]
  /** encounter cards attached to it (Restrained) */
  attachments?: AttachmentView[]
}

export interface PlayerView {
  id: string
  name: string
  form: 'hero' | 'alterEgo' | string
  code: string
  otherCode: string
  /** Every side of the identity card, A then B then C (Ant-Man's fold-out Giant). */
  faces: string[]
  hitPoints: number
  damage: number
  exhausted: boolean
  stunned: boolean
  confused: boolean
  tough: boolean
  /** on the identity card: Groot's growth counters, Drax's vengeance */
  counters?: number
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
  /** the encounter cards dealt face down, by id */
  dealtCardIds?: string[]
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
  /** infinite hit points (Escape the Museum): shown as ∞ */
  infiniteHitPoints?: boolean
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
  kind: 'villain' | 'named' | 'sideScheme' | 'infinityStones' | string
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
  /** the acceleration tokens adding threat to its main scheme */
  accelerationTokens: number
  /** the acceleration icons on cards in play, each a threat more each villain phase */
  accelerationIcons?: number
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
  /** the cards facedown beneath it */
  held?: number
  /** which, and whose */
  heldCards?: { id: string; owner: string | null }[]
  /** the player cards attached to it faceup (Odin on Odin's Torment) */
  attachedCards?: CardView[]
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
  /** `$story.…` keys to read before answering: a campaign scenario's story and rules */
  reading?: string[]
  /** the cards searched, those not among the choices shown too, by where they were */
  searched?: SearchedView[]
}

/** A card or ability being paid for, and the resources paid so far. */
export interface ActiveCostView {
  canceledBy?: ('stunned' | 'confused')[]
  player: string
  /** the card being played */
  card: CardView | null
  /** or the card whose ability is being paid for, by id */
  source: string | null
  paid: string[]
  /** which of the paid resources are wild ones, shown as the type declared */
  wilds?: number[]
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
  /** the player in whose play area it is (Ebony Maw's Spells); none: the villain's */
  owner?: string | null
  /** the damage on it (Tower Defense's Avengers Tower) */
  damage?: number
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
  /** the victory display: cards with victory X, defeated, the first added first */
  victoryDisplay?: CardView[]
  /** how many villains in the victory display win the game, when the players chose it (Loki) */
  victoryCondition?: number | null
  /** The Collection (Infiltrate the Museum): player and encounter cards, faceup, the first added first */
  collection?: CardView[]
  focusedCards: CardView[]
  /** an enemy's attack, while it is made: ids of who attacks and who is attacked */
  attack?: { attacker: string; target: string } | null
  /** threat that would be placed on a scheme, while its window is open */
  threat?: { scheme: string; amount: number; source: 'ThreatFromVillain' | 'ThreatFromMinion' | 'ThreatFromAbility' | 'ThreatFromAcceleration'; threshold?: number | null } | null
  /** boost cards: face down, without a code, until flipped */
  boostCards?: BoostView[]
  accelerationTokens: number
  /** the acceleration icons on cards in play, each a threat more each villain phase */
  accelerationIcons?: number
  question: Record<string, QuestionView>
  questionVersion: number
  activeCost?: ActiveCostView | null
  areas?: AreaView[]
  /** the area of the first seat whose hand you see */
  viewerArea?: number | null
  /** the campaign the game is a scenario of */
  campaign?: CampaignInfo | null
  [more: string]: unknown
}

/** Who plays: with friends, who sits where (seat n plays the nth player); otherwise the owner plays every seat. */
export interface Table {
  withFriends: boolean
  seats: number
  owner: number
  seated: { position: number; userId: number; username: string; hero?: string | null }[]
  /** the campaign the game is a scenario of */
  campaign?: number | null
}

export interface GameData {
  started: boolean
  game: GameView | null
  playerId: string | null
  canUndo: boolean
  table: Table
  /** the scenario as it was chosen, for the lobby to show before it is dealt */
  setup: { scenarioCode: string; modulars: string[]; difficulty: 'Normal' | 'Expert' }
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
  | { tag: 'EventCanceled'; code: string; statuses: ('stunned' | 'confused')[] }

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

/** Which campaign a game is a scenario of, and which of its scenarios (from 1). */
export interface CampaignInfo {
  name: string | null
  mode: CampaignMode
  scenario: number
  scenarios: number
}

/** A choice a campaign victory leaves a player not in the game as it was won, asked as the next game is set up. */
export type CampaignChoice = 'ChooseTechUpgrade' | 'ChooseBasicUpgrade' | 'MayImproveUpgrade'

/** A column of the campaign log's Player Information. */
export interface CampaignPlayer {
  /** the hero face's code */
  identity: string
  number: number
  hitPoints: number | null
  obligations: string[]
  techUpgrade: string | null
  conditionUpgrade: string | null
  rescuedAllies: string[]
  pending: CampaignChoice[]
}

export interface CampaignLog {
  players: CampaignPlayer[]
  experimentalWeapons: string[]
  delayCounters: number | null
  engagedWithMinions: string[]
  alliesRemoved: string[]
  /** The Galaxy's Most Wanted's */
  galaxy: GalaxyLog
  /** The Mad Titan's Shadow's */
  titan: TitanLog
}

/** A box of The Mad Titan's Shadow's log: the campaign pool and what the scenarios record. */
export type TitanMark =
  | 'CosmoInPool'
  | 'SecurityBreachInPool'
  | 'ShawarmaInPool'
  | 'BlackSwanInPool'
  | 'AvengersTowerDamaged'
  | 'SystemShockInPool'
  | 'InfinityStones1BCompleted'
  | 'NornStoneInPool'
  | 'OdinInPool'

export interface TitanLog {
  marks: TitanMark[]
}

/** A player's column of The Galaxy's Most Wanted's log, besides identity and hit points. */
export interface GalaxyPlayer {
  identity: string
  units: number
  marketCards: string[]
  collection: string[]
}

export interface GalaxyLog {
  galaxyPlayers: GalaxyPlayer[]
  galacticArtifacts: string[]
  powerStoneControl: string | null
  /** the scenarios (1 to 4) Badoon Headhunter was in the victory display of */
  headhunterDefeated: number[]
  evasionCounters: number | null
}

/** Which box's campaign. */
export type CampaignKind = 'RiseOfRedSkull' | 'GalaxysMostWanted' | 'MadTitansShadow'

export type CampaignMode = 'StandardCampaign' | 'ExpertCampaign'

/** The campaign as the server's Marvel.Campaign has it. */
export interface Campaign {
  mode: CampaignMode
  kind: CampaignKind
  /** the scenarios in order, by villain code */
  scenarios: string[]
  completed: number
  lost: boolean
  campaignLog: CampaignLog
}

/** A campaign as its overview shows it. */
export interface CampaignView {
  id: string
  name: string
  campaign: Campaign
  difficulty: 'Normal' | 'Expert'
  heroes: string[]
  /** its seats, as a game's lobby shows them */
  table: Table
  /** with friends, seats still open before its first game */
  waitingForPlayers: boolean
  /** for each seat taken, how many cards its deck must gain before it is dealt */
  deckShort: number[]
  /** The Galaxy's Most Wanted: The Market's cards not yet bought, with their unit cost */
  market: [string, number][]
  next: string | null
  nextName: string | null
  /** the game of it being played, not yet over */
  currentGame: string | null
  won: boolean
  updatedAt: string
}
