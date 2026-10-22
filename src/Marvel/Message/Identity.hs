module Marvel.Message.Identity where

import Marvel.Prelude

import Marvel.Attack
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
  | EndedTurn
  | ChangedToForm Side
  | DrawCards FromZone Natural
  | DiscardCards
  | DiscardFor Target FromZone Natural Natural
  | DiscardedFor Target FromZone Natural Natural [Card]
  | ShuffleDeck
  | CheckAllyLimit
  | -- | over two restricted cards, one is discarded
    CheckRestrictedLimit
  | SideMessage SideMessage
  | PlayedCard PlayerCard (Maybe Window)
  | PaidWithCard PlayerCard
  | AllyCreated AllyId
  | AllyRemoved AllyId
  | UpgradeCreated UpgradeId
  | SupportCreated SupportId
  | SupportRemoved SupportId
  | AddToHand PlayerCard
  | ShuffleIntoIdentityDeck [PlayerCard]
  | ChooseFromDiscard Target ChoiceRules Natural Natural
  | ChosenFromDiscard Target ChoiceRules Natural Natural [PlayerCard]
  | DiscardFrom FromZone Natural (Maybe Target)
  | ExhaustedIdentity
  | ReadiedIdentity
  | DrawOrDiscardToHandLimit
  | DrawToHandLimit
  | -- | the hand size printed on the identity, without what modifies it
    DrawToPrintedHandSize
  | ReadyCards
  | SetupIdentity
  | DealStartingHand
  | FinishedSearching
  | VillainAndMinionsActivate
  | DealtEncounterCard EncounterCard
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
  | IdentityRetaliate Natural EnemyId
  | -- | the hit point dial is set: the damage is what leaves this many hit points
    IdentitySetHitPoints Natural
  | -- | discard from the top of the deck until a card matching is discarded, which the
    -- target is told of; the deck emptying ends it
    DiscardFromDeckUntil CardMatcher Target
  | PlaceOnBottomOfDeck PlayerCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage
  = Recovered
  | Attacked
  | Thwarted
  | Defended EnemyId
  | -- | the basic attack, once "when you make a basic attack" has been answered
    BasicAttackDamage
  | -- | the defender's DEF is applied, once "when defends" is answered
    DefenseApplied EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
