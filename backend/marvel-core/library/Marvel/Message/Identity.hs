module Marvel.Message.Identity where

import Marvel.Prelude

import Marvel.Attack
import Marvel.Card.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Message.Deck
import Marvel.Source
import Marvel.Target
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
  | ReadyCards
  | SetupIdentity
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage
  = Recovered
  | Attacked
  | Thwarted
  | Defended EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


