{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Marvel.Message where

import Marvel.Prelude

import GHC.Generics
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Phase
import {-# SOURCE #-} Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Window (Window)

data FromZone = FromDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FinishedStatus = Won | Lost
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
  | IdentityEndedTurn IdentityId
  | AddVillain CardCode
  | SetPlayerOrder [IdentityId]
  | IdentityMessage IdentityId IdentityMessage
  | VillainMessage VillainId VillainMessage
  | EventMessage EventId EventMessage
  | EffectMessage EffectId EffectMessage
  | AllyMessage AllyId AllyMessage
  | SupportMessage SupportId SupportMessage
  | UpgradeMessage UpgradeId UpgradeMessage
  | MainSchemeMessage CardCode MainSchemeMessage
  | SideSchemeMessage SideSchemeId SideSchemeMessage
  | TreacheryMessage TreacheryId TreacheryMessage
  | MinionMessage MinionId MinionMessage
  | AttachmentMessage AttachmentId AttachmentMessage
  | Ask IdentityId Question
  | UsedAbility IdentityId Ability
  | RanAbility Target Natural [Window]
  | WithDiscarded Target FromZone [PlayerCard]
  | SetActiveCost ActiveCost
  | Spent PlayerCard
  | Paid Payment
  | FinishedPayment
  | PutCardIntoPlay IdentityId PlayerCard Payment (Maybe Window)
  | CheckWindows [Window]
  | EndCheckWindows
  | DealBoost Target
  | FlipBoostCards Target
  | DealEncounterCard IdentityId
  | Surge IdentityId
  | DiscardedEncounterCard EncounterCard
  | DeclareDefense IdentityId EnemyId
  | RemoveFromPlay Target
  | CreatedEffect CardDef Source EntityMatcher
  | DisabledEffect EffectId
  | RevealEncounterCard IdentityId EncounterCard
  | AdvanceScenario
  | GameOver FinishedStatus
  | UpgradeRemoved UpgradeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data VillainMessage
  = SetVillainHp
  | VillainDamaged Source Natural
  | VillainStunned Source
  | VillainConfused Source
  | DealtBoost EncounterCard
  | VillainFlipBoostCards
  | VillainAttacks IdentityId
  | VillainBeginAttack IdentityId
  | VillainAttacked
  | VillainSchemes
  | VillainSchemed
  | VillainHealed Natural
  | VillainDefendedBy CharacterId
  | AttachedToVillain AttachmentId
  | UpgradeAttachedToVillain UpgradeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MainSchemeMessage = MainSchemeThwarted Source Natural | MainSchemePlaceThreat Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideSchemeMessage = RevealSideScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TreacheryMessage = RevealTreachery IdentityId | ResolvedTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MinionMessage
  = RevealMinion
  | MinionDefendedBy CharacterId
  | MinionDamaged Source Natural
  | MinionStunned Source
  | MinionConfused Source
  | MinionDefeated
  | MinionHealed Natural
  | UpgradeAttachedToMinion UpgradeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AttachmentMessage = RevealAttachment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventMessage = PlayedEvent IdentityId Payment (Maybe Window) | ResolvedEvent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EffectMessage = DisableEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AllyMessage
  = ExhaustedAlly
  | ReadiedAlly
  | AllyAttacked
  | AllyThwarted
  | AllyDamaged Source Natural
  | AllyDefended EnemyId
  | AllyDefeated
  | AllyHealed Natural
  | SpendAllyUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SupportMessage
  = ExhaustedSupport
  | ReadiedSupport
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpgradeMessage
  = ExhaustedUpgrade
  | ReadiedUpgrade
  | PlayedUpgrade
  | AttachedToEnemy EnemyId
  | SpendUpgradeUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IdentityMessage
  = BeginTurn
  | PlayerTurnOption
  | CheckIfPassed
  | ChooseOtherForm
  | EndedTurn
  | ChangedToForm Side
  | DrawCards FromZone Natural
  | DiscardCards
  | ShuffleDeck
  | SideMessage SideMessage
  | PlayedCard PlayerCard (Maybe Window)
  | PaidWithCard PlayerCard
  | AllyCreated AllyId
  | AllyRemoved AllyId
  | UpgradeCreated UpgradeId
  | SupportCreated SupportId
  | SupportRemoved SupportId
  | AddToHand PlayerCard
  | DiscardFrom FromZone Natural (Maybe Target)
  | DiscardCard PlayerCard
  | ExhaustedIdentity
  | DrawOrDiscardToHandLimit
  | ReadyCards
  | SetupIdentity
  | VillainAndMinionsActivate
  | DealtEncounterCard EncounterCard
  | RevealEncounterCards
  | IdentityDamaged Source Natural
  | IdentityDefended Natural
  | IdentityHealed Natural
  | MinionEngaged MinionId
  | MinionDisengaged MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage
  = Recovered
  | Attacked
  | Thwarted
  | Defended EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f
instance (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f

class RunMessage a where
  runMessage :: (MonadGame env m, CoerceRole m) => Message -> a -> m a

class RunMessage' f where
  runMessage' :: (MonadGame env m, CoerceRole m) => Message -> f p -> m (f p)

genericRunMessage
  :: (MonadGame env m, RunMessage' (Rep a), Generic a, CoerceRole m)
  => Message
  -> a
  -> m a
genericRunMessage msg = fmap to . runMessage' msg . from

instance RunMessage' f => RunMessage' (M1 i c f) where
  runMessage' msg = fmap M1 . runMessage' msg . unM1

instance (RunMessage' l, RunMessage' r) => RunMessage' (l :+: r) where
  runMessage' msg = \case
    L1 x -> L1 <$> runMessage' msg x
    R1 x -> R1 <$> runMessage' msg x

instance RunMessage c => RunMessage' (K1 i c) where
  runMessage' msg = fmap K1 . runMessage msg . unK1
