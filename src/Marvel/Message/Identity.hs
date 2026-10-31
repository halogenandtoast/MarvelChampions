module Marvel.Message.Identity where

import Marvel.Prelude

import Marvel.Attack
import Marvel.Card.Code (CardCode)
import Marvel.Card.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Message.Deck
import Marvel.Ref
import Marvel.Window.Types (Window)

data IdentityMessage
  = BeginTurn
  | PlayerTurnOption
  | CheckIfPassed
  | ChooseOtherForm
  | -- | change to a hero form other than the one showing: Ant-Man's other size, or from
    -- alter-ego the player's pick of their hero forms
    ChooseHeroForm
  | EndedTurn
  | ChangedToForm Side
  | -- | Replace both faces without changing form or resetting identity state.
    SwapIdentityVersion CardCode CardCode
  | DrawCards FromZone Natural
  | DiscardCards
  | DiscardFor Target FromZone Natural Natural
  | DiscardedFor Target FromZone Natural Natural [Card]
  | ShuffleDeck
  | CheckAllyLimit
  | -- | over two restricted cards, one is discarded
    CheckRestrictedLimit
  | SideMessage SideMessage
  | -- | the player begins to play the card: they choose what it is played on, then pay
    PlayingCard PlayerCard (Maybe Window)
  | PayForCard PlayerCard (Maybe Window) (Maybe Target)
  | -- | its cost is paid: it is played
    PlayedCard PlayerCard (Maybe Window)
  | PaidWithCard PlayerCard
  | AllyCreated AllyId
  | AllyRemoved AllyId
  | UpgradeCreated UpgradeId
  | -- | A controlled upgrade changes controller without leaving play.
    UpgradeControlLost UpgradeId
  | SupportCreated SupportId
  | SupportRemoved SupportId
  | AddToHand PlayerCard
  | ShuffleIntoIdentityDeck [PlayerCard]
  | ChooseFromDiscard Target ChoiceRules Natural Natural
  | ChosenFromDiscard Target ChoiceRules Natural Natural [PlayerCard]
  | DiscardFrom FromZone Natural (Maybe Target)
  | ExhaustedIdentity
  | ReadiedIdentity
  | -- | readied by the game or an encounter card, not a player card (Unnatural Storm)
    ReadiedIdentityByGame
  | DrawOrDiscardToHandLimit
  | DrawToHandLimit
  | -- | the hand size printed on the identity, without what modifies it
    DrawToPrintedHandSize
  | ReadyCards
  | SetUpWeatherDeck [PlayerCard]
  | ChooseWeatherSupport PlayerCard Bool
  | SetupIdentity
  | -- | the cards with the setup keyword come out of the deck into play
    PutSetupCardsIntoPlay
  | DealStartingHand
  | FinishedSearching
  | VillainAndMinionsActivate
  | -- | the minions engaged with the player activate, those put into play as the
    -- villain activated among them
    MinionsActivate
  | DealtEncounterCard EncounterCard
  | IdentityDealtBoost EncounterCard
  | EncounterCardInDiscard EncounterCard
  | MoveIdentityBoostCardsToVillain VillainId
  | RevealEncounterCards
  | IdentityWasAttacked Attack
  | IdentityDamaged Source Damage
  | IdentityDefended Natural
  | IdentityDefeated
  | IdentityHealed Natural
  | IdentityStunned
  | IdentityConfused
  | IdentityRemoveStunned
  | IdentityRemoveConfused
  | MinionEngaged MinionId
  | MinionDisengaged MinionId
  | ShuffleIdentityDiscardBackIntoDeck
  | Search SearchSignifier CardMatcher SearchOption ReturnOption
  | -- | after the attack resolves, the retaliate the identity has then
    IdentityRetaliate EnemyId
  | -- | the hit point dial is set: the damage is what leaves this many hit points
    IdentitySetHitPoints Natural
  | -- | discard from the top of the deck until a card matching is discarded, which the
    -- target is told of; the deck emptying ends it
    DiscardFromDeckUntil CardMatcher Target
  | PlaceOnBottomOfDeck PlayerCard
  | PlaceOnTopOfDeck PlayerCard
  | IdentityBecomeTough
  | -- | the attack the hero defended is done with them: their damage before it, to tell
    -- whether they took any from it
    AttackDefended EnemyId Natural
  | -- | so many counters placed on the identity card, to at most the second number
    PlaceCountersUpTo Natural Natural
  -- Infiltrate the Museum
  | -- | the top card of the deck, as it is then, goes faceup into The Collection
    TopOfDeckIntoCollection
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage
  = Recovered
  | Attacked
  | Thwarted
  | Defended EnemyId
  | DefendedWithoutExhausting Natural EnemyId
  | DefenseUsingAttackApplied Natural EnemyId
  | -- | the basic attack, after the interrupts to making it, and what they add to it
    BasicAttack Natural
  | -- | the basic attack's target chosen, after the interrupts to attacking it
    BasicAttackOn EnemyId Natural
  | -- | the basic thwart, after the interrupts to making it, and what they add to it
    BasicThwart Natural
  | -- | the defender's DEF, given the bonus, is applied once "when defends" is answered
    DefenseApplied Natural EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
