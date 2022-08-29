module Marvel.Message
  ( module Marvel.Message
  , module X
  ) where

import Marvel.Prelude

import Marvel.Message.Ally as X
import Marvel.Message.Attachment as X
import Marvel.Message.Deck as X
import Marvel.Message.Effect as X
import Marvel.Message.Event as X
import Marvel.Message.Identity as X
import Marvel.Message.MainScheme as X
import Marvel.Message.Minion as X
import Marvel.Message.Obligation as X
import Marvel.Message.SideScheme as X
import Marvel.Message.Support as X
import Marvel.Message.Treachery as X
import Marvel.Message.Upgrade as X
import Marvel.Message.Villain as X

import Marvel.Ability.Types
import Marvel.ActiveCost.Types
import Marvel.Card.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Payment.Types
import Marvel.Phase
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Projection
import Marvel.Identity.Types
import Marvel.Source
import Marvel.Target
import Marvel.Window.Types (Window, WindowType)

class RunMessage a where
  runMessage :: (HasCallStack, MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) => Message -> a -> m a

data FinishedStatus
  = Won
  | Lost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DefensePriority
  = AnyDefense
  | AllyIfAble
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = StartGame
  | StartScenario
  | BeginPhase Phase
  | EndPhase Phase
  | PassFirstPlayer
  | EndRound
  | BeginRound
  | AccelerateMainScheme
  | AddAccelerationToken
  | IdentityEndedTurn IdentityId
  | AddVillain CardCode
  | AddMainScheme CardCode
  | ReplaceMainScheme CardCode
  | SetupMainScheme
  | SetPlayerOrder [IdentityId]
  | IdentityMessage IdentityId IdentityMessage
  | VillainMessage VillainId VillainMessage
  | EventMessage EventId EventMessage
  | EffectMessage EffectId EffectMessage
  | AllyMessage AllyId AllyMessage
  | SupportMessage SupportId SupportMessage
  | UpgradeMessage UpgradeId UpgradeMessage
  | MainSchemeMessage MainSchemeId MainSchemeMessage
  | SideSchemeMessage SideSchemeId SideSchemeMessage
  | TreacheryMessage TreacheryId TreacheryMessage
  | ObligationMessage ObligationId ObligationMessage
  | MinionMessage MinionId MinionMessage
  | AttachmentMessage AttachmentId AttachmentMessage
  | Ask IdentityId Question
  | UsedAbility IdentityId Ability
  | RanAbility Target Natural [WindowType] Payment
  | SearchFoundCards Target [Card]
  | DiscardTopOfEncounterDeck Natural (Maybe Target)
  | WithDiscarded Target FromZone [Card]
  | WithChosen Target FromZone [Card]
  | SetActiveCost ActiveCost
  | CreatedActiveCost ActiveCostId
  | DisableActiveCost ActiveCostId
  | Spent ActiveCostId PlayerCard
  | Paid Payment
  | FinishedPayment ActiveCostId
  | PutCardIntoPlay IdentityId PlayerCard Payment (Maybe Window)
  | CheckWindows [Window]
  | EndCheckWindows
  | EmptyScenarioDeck
  | DealBoost Target
  | FlipBoostCards Target
  | PutBoostIntoPlay Target IdentityId
  | DealEncounterCard IdentityId
  | GainSurge Target
  | Surge IdentityId
  | DrawAndRevealEncounterCard IdentityId
  | DiscardedCard Card
  | DeclareDefense IdentityId EnemyId DefensePriority
  | RemoveFromPlay Target
  | RemoveFromGame Target
  | CreatedEffect CardDef Source EntityMatcher
  | DisabledEffect EffectId
  | RevealEncounterCard IdentityId EncounterCard
  | RevealedEncounterCard IdentityId EncounterCard
  | RevealBoostCard EncounterCard EnemyId
  | AdvanceMainScheme
  | NextMainScheme
  | GameOver FinishedStatus
  | UpgradeRemoved UpgradeId
  | AttachmentRemoved AttachmentId
  | FocusCards [Card]
  | UnfocusCards
  | SearchForAndRevealScheme CardDef
  | ShuffleEncounterDeck
  | ReturnToHand Target
  | SetAside [Card]
  | ShuffleIntoEncounterDeck [EncounterCard]
  | ClearBoosts
  | ClearRemoved
  | CheckPayment ActiveCostId
  | Boost Message
  | RevealedAsBoost Target EnemyId
  | ChoseEnemy EnemyId Target
  | ChosePlayer IdentityId Target
  | ChoseUpgrade UpgradeId Target
  | DiscardUntil FromZone CardMatcher Target
  | WithDiscardedMatch Target FromZone Card
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
