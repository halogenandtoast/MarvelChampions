{-# LANGUAGE FlexibleInstances #-}

module Marvel.Message where

import Marvel.Prelude

import GHC.Generics
import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Phase
import {-# SOURCE #-} Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Window (Window, WindowType)

data FromZone = FromDeck | FromHand | FromDiscard | RandomFromHand | FromEncounterDeck
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
  | RanAbility Target Natural [WindowType]
  | SearchFoundCards Target [Card]
  | DiscardTopOfEncounterDeck Natural (Maybe Target)
  | WithDiscarded Target FromZone [Card]
  | WithChosen Target FromZone [Card]
  | SetActiveCost ActiveCost
  | Spent PlayerCard
  | Paid Payment
  | FinishedPayment
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
  | DeclareDefense IdentityId EnemyId
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
  | Boost Message
  | RevealedAsBoost Target EnemyId
  | ChoseEnemy EnemyId Target
  | ChosePlayer IdentityId Target
  | ChoseUpgrade UpgradeId Target
  | DiscardUntil FromZone CardMatcher (Maybe Target)
  | WithDiscardedMatch Target FromZone Card
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data VillainMessage
  = SetVillainHp
  | VillainDamaged Source Damage
  | VillainStunned Source
  | VillainConfused Source
  | VillainBecomeTough
  | VillainDealtBoost EncounterCard
  | VillainFlipBoostCards
  | VillainCheckAdditionalBoosts
  | VillainAttackGainOverkill
  | VillainAttacks IdentityId
  | VillainBeginAttack IdentityId
  | VillainEndAttack
  | VillainAttacked
  | VillainSchemes
  | VillainSchemed
  | VillainHealed Natural
  | VillainDefeated
  | VillainDefendedBy CharacterId
  | AttachedToVillain AttachmentId
  | AttachedUpgradeToVillain UpgradeId
  | VillainAdvanced
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MainSchemeMessage
  = MainSchemeThwarted Source Natural
  | MainSchemePlaceThreat Natural
  | MainSchemePlaceInitialThreat
  | RevealMainScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideSchemeMessage
  = RevealSideScheme
  | DefeatSideScheme
  | SideSchemePlaceInitialThreat
  | SideSchemePlaceThreat Natural
  | SideSchemeThwarted Source Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TreacheryMessage
  = RevealTreachery IdentityId
  | ResolvedTreachery IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ObligationMessage
  = RevealObligation IdentityId
  | ResolveObligation IdentityId
  | ResolvedObligation IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MinionMessage
  = RevealMinion IdentityId
  | MinionDefendedBy CharacterId
  | MinionDamaged Source Damage
  | MinionStunned Source
  | MinionConfused Source
  | MinionBecomeTough
  | MinionDefeated
  | MinionHealed Natural
  | MinionHealAllDamage
  | MinionAttacks IdentityId
  | MinionSchemes
  | MinionSchemed
  | MinionBeginAttack IdentityId
  | MinionEngagedIdentity IdentityId
  | MinionAttacked
  | AttachedUpgradeToMinion UpgradeId
  | AttachedToMinion AttachmentId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AttachmentMessage
  = RevealAttachment IdentityId
  | AttachmentDamaged Natural
  | AttachedToEnemy EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventMessage = PlayedEvent IdentityId Payment (Maybe WindowType) | ResolvedEvent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EffectMessage = DisableEffect | UsedEffect IdentityId | EffectChoice Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AllyMessage
  = ExhaustedAlly
  | ReadiedAlly
  | AllyAttacked
  | AllyThwarted
  | AllyWasAttacked Attack
  | AllyDamaged Source Damage
  | AllyDefended EnemyId
  | AllyDefeated
  | AllyHealed Natural
  | SpendAllyUse
  | AttachedUpgradeToAlly UpgradeId
  | AllyStunned
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SupportMessage
  = ExhaustedSupport
  | ReadiedSupport
  | SpendSupportUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpgradeMessage
  = ExhaustedUpgrade
  | ReadiedUpgrade
  | PlayedUpgrade
  | UpgradeAttachedToEnemy EnemyId
  | UpgradeAttachedToAlly AllyId
  | SpendUpgradeUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchOption = SearchTarget Target | SearchDrawOne
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ChoiceRules = DifferentCards
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

data SearchSignifier = SearchIdentityDeck IdentityId DeckProjection
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DeckProjection = AllOfDeck | TopOfDeck Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReturnOption = ShuffleBackIn | DiscardRest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage
  = Recovered
  | Attacked
  | Thwarted
  | Defended EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class RunMessage a where
  runMessage :: MonadGame env m => Message -> a -> m a

class RunMessage' f where
  runMessage' :: MonadGame env m => Message -> f p -> m (f p)

genericRunMessage ::
  (MonadGame env m, RunMessage' (Rep a), Generic a) => Message -> a -> m a
genericRunMessage msg = fmap to . runMessage' msg . from

instance RunMessage' f => RunMessage' (M1 i c f) where
  runMessage' msg = fmap M1 . runMessage' msg . unM1

instance (RunMessage' l, RunMessage' r) => RunMessage' (l :+: r) where
  runMessage' msg = \case
    L1 x -> L1 <$> runMessage' msg x
    R1 x -> R1 <$> runMessage' msg x

instance RunMessage c => RunMessage' (K1 i c) where
  runMessage' msg = fmap K1 . runMessage msg . unK1
