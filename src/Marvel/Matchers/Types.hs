{-# LANGUAGE PatternSynonyms #-}

module Marvel.Matchers.Types where

import Marvel.Prelude

import Marvel.Ability.Types
import {-# SOURCE #-} Marvel.Card.Def
import {-# SOURCE #-} Marvel.Card.PlayerCard.Types
import Marvel.GameValue.Types
import Marvel.Id
import Marvel.Keyword
import Marvel.Name
import Marvel.Status
import Marvel.Trait.Types

data EntityMatcher
  = IdentityEntity IdentityMatcher
  | AllyEntity AllyMatcher
  | AttachmentEntity AttachmentMatcher
  | -- | an event being played
    EventEntity EventId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data IdentityMatcher
  = HeroIdentity
  | AlterEgoIdentity
  | UnexhaustedIdentity
  | ExhaustedIdentity
  | ConfusedIdentity
  | StunnedIdentity
  | IdentityWithTitle Text
  | IdentityWithId IdentityId
  | IdentityWithTrait Trait
  | AnyIdentity
  | IdentityWithDamage GameValueMatcher
  | IdentityEngagedWith MinionMatcher
  | You
  | FirstPlayer
  | IdentityMatchAll [IdentityMatcher]
  | -- | not kept from thwarting (by a 'CannotThwart' modifier)
    IdentityCanThwart
  | -- | not kept in its form (by a 'CannotChangeForm' modifier)
    IdentityCanChangeForm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Monoid IdentityMatcher where
  mempty = AnyIdentity

instance Semigroup IdentityMatcher where
  AnyIdentity <> x = x
  x <> AnyIdentity = x
  IdentityMatchAll xs <> IdentityMatchAll ys = IdentityMatchAll $ xs <> ys
  IdentityMatchAll xs <> y = IdentityMatchAll $ xs <> [y]
  x <> IdentityMatchAll ys = IdentityMatchAll $ x : ys
  x <> y = IdentityMatchAll [x, y]

pattern IdentityWithAnyDamage :: IdentityMatcher
pattern IdentityWithAnyDamage <-
  IdentityWithDamage (GreaterThan (Static 0))
  where
    IdentityWithAnyDamage = IdentityWithDamage (GreaterThan (Static 0))

data AllyMatcher
  = UnexhaustedAlly
  | ExhaustedAlly
  | AllyWithUses GameValueMatcher
  | AllyControlledBy IdentityMatcher
  | AllyWithDamage GameValueMatcher
  | AllyWithId AllyId
  | AllyWithUpgrade UpgradeMatcher
  | AnyAlly
  | NotAlly AllyMatcher
  | AllyMatches [AllyMatcher]
  | AllyWithTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AllyWithAnyUses :: AllyMatcher
pattern AllyWithAnyUses <- AllyWithUses (GreaterThan (Static 0))
  where
    AllyWithAnyUses = AllyWithUses (GreaterThan (Static 0))

instance Semigroup AllyMatcher where
  AnyAlly <> x = x
  x <> AnyAlly = x
  AllyMatches xs <> AllyMatches ys = AllyMatches $ xs <> ys
  x <> AllyMatches ys = AllyMatches $ x : ys
  AllyMatches xs <> y = AllyMatches $ xs <> [y]
  x <> y = AllyMatches [x, y]

data SupportMatcher
  = UnexhaustedSupport
  | SupportControlledBy IdentityMatcher
  | SupportWithUses GameValueMatcher
  | SupportNamed Name
  | SupportMatches [SupportMatcher]
  | SupportWithTrait Trait

pattern SupportWithAnyUses :: SupportMatcher
pattern SupportWithAnyUses <- SupportWithUses (GreaterThan (Static 0))
  where
    SupportWithAnyUses = SupportWithUses (GreaterThan (Static 0))

pattern UpgradeWithAnyUses :: UpgradeMatcher
pattern UpgradeWithAnyUses <- UpgradeWithUses (GreaterThan (Static 0))
  where
    UpgradeWithAnyUses = UpgradeWithUses (GreaterThan (Static 0))

upgradeControlledBy :: IdentityId -> UpgradeMatcher
upgradeControlledBy = UpgradeControlledBy . IdentityWithId

data UpgradeMatcher
  = UpgradeWithUses GameValueMatcher
  | UpgradeControlledBy IdentityMatcher
  | UpgradeWithTrait Trait
  | UpgradeWithKeyword Keyword
  | UpgradeIs CardDef
  | UpgradeNamed Name
  | UnexhaustedUpgrade
  | UpgradeWithId UpgradeId
  | UpgradeMatches [UpgradeMatcher]
  | UpgradeOneOf [UpgradeMatcher]
  | -- | on an identity matching: controlled by it, and attached to nothing else
    UpgradeOnIdentity IdentityMatcher
  | -- | of the upgrades matching, those with the highest printed cost
    UpgradeWithHighestCostAmong UpgradeMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup UpgradeMatcher where
  UpgradeMatches xs <> UpgradeMatches ys = UpgradeMatches $ xs <> ys
  x <> UpgradeMatches ys = UpgradeMatches $ x : ys
  UpgradeMatches xs <> y = UpgradeMatches $ xs <> [y]
  x <> y = UpgradeMatches [x, y]

data EnemyMatcher
  = AnyEnemy
  | EnemyWithId EnemyId
  | VillainEnemy
  | MinionEnemy
  | -- | an enemy a player matching may attack: guard keeps the villain from them
    AttackableBy IdentityMatcher
  | DamageableEnemy
  | EnemyIs CardDef
  | NotEnemy EnemyMatcher
  | UndefendedEnemy
  | EnemyWithUpgrade UpgradeMatcher
  | EnemyMatches [EnemyMatcher]
  | DefeatedEnemy EnemyMatcher
  | -- | an enemy whose attack is under way
    AttackingEnemy
  | -- | the enemies whose ATK is the highest of all enemies'
    EnemyWithHighestAttack
  | StunnedEnemy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatches xs <> EnemyMatches ys = EnemyMatches (xs <> ys)
  EnemyMatches xs <> y = EnemyMatches (xs <> [y])
  x <> EnemyMatches ys = EnemyMatches (x : ys)
  x <> y = EnemyMatches [x, y]

pattern AttackableEnemy :: EnemyMatcher
pattern AttackableEnemy = AttackableBy You

pattern VillainWithAnyDamage :: VillainMatcher
pattern VillainWithAnyDamage <-
  VillainWithDamage (GreaterThan (Static 0))
  where
    VillainWithAnyDamage = VillainWithDamage (GreaterThan (Static 0))

data VillainMatcher
  = ActiveVillain
  | AnyVillain
  | VillainWithId VillainId
  | VillainWithDamage GameValueMatcher
  | VillainWithToughStatus
  | -- | with the trait, printed or gained
    VillainWithTrait Trait
  | VillainMatches [VillainMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup VillainMatcher where
  VillainMatches xs <> VillainMatches ys = VillainMatches $ xs <> ys
  x <> VillainMatches ys = VillainMatches $ x : ys
  VillainMatches xs <> y = VillainMatches $ xs <> [y]
  x <> y = VillainMatches [x, y]

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SchemeMatcher = AnyScheme | MainScheme | ThwartableScheme | SchemeWithId SchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SideSchemeMatcher
  = AnySideScheme
  | CrisisSideScheme
  | SideSchemeIs CardDef
  | SideSchemeWithId SideSchemeId
  | SideSchemeWithUpgrade UpgradeMatcher
  | NotSideScheme SideSchemeMatcher
  | -- | the side scheme a villain matching owns, as Wrecker's Day of Reckoning
    SignatureSchemeOf VillainMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data MinionMatcher
  = AnyMinion
  | MinionWithId MinionId
  | MinionWithDamage GameValueMatcher
  | MinionWithTrait Trait
  | MinionWithKeyword Keyword
  | MinionEngagedWith IdentityMatcher
  | MinionIs CardDef
  | MinionWithHighestPrintedHitPoints
  | MinionMatches [MinionMatcher]
  | NotMinion MinionMatcher
  | MinionWithAttachment AttachmentMatcher
  | StunnedMinion
  | -- | of the minions matching, those with the highest printed hit points
    MinionWithHighestPrintedHitPointsAmong MinionMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup MinionMatcher where
  AnyMinion <> x = x
  x <> AnyMinion = x
  MinionMatches xs <> MinionMatches ys = MinionMatches $ xs <> ys
  x <> MinionMatches ys = MinionMatches $ x : ys
  MinionMatches xs <> y = MinionMatches $ xs <> [y]
  x <> y = MinionMatches [x, y]

data GameValueMatcher = AnyValue | GreaterThan GameValue | AtLeast GameValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern CharacterWithAnyDamage :: CharacterMatcher
pattern CharacterWithAnyDamage <-
  CharacterWithDamage (GreaterThan (Static 0))
  where
    CharacterWithAnyDamage = CharacterWithDamage (GreaterThan (Static 0))

data CharacterMatcher
  = CharacterWithDamage GameValueMatcher
  | DamageableCharacter
  | AnyCharacter
  | CharacterMatches [CharacterMatcher]
  | -- | the identities and allies of the players matching
    CharacterControlledBy IdentityMatcher
  | CharacterWithTrait Trait
  | CharacterWithStatus StatusCard
  | -- | of the heroes and allies, those whose ATK is the highest
    HeroOrAllyWithHighestAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CharacterMatcher where
  AnyCharacter <> x = x
  x <> AnyCharacter = x
  CharacterMatches xs <> CharacterMatches ys = CharacterMatches $ xs <> ys
  x <> CharacterMatches ys = CharacterMatches $ x : ys
  CharacterMatches xs <> y = CharacterMatches $ xs <> [y]
  x <> y = CharacterMatches [x, y]

newtype EncounterCardMatcher = NemesisSetFor IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data BasicEncounterCardMatcher
  = AnyEncounterCard
  | EncounterCardMatching CardMatcher
  | EncounterCardWithBoostIcons
  | -- | a star in its boost field: a "Boost" ability
    EncounterCardWithBoostAbility
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ExtendedCardMatcher
  = AffordableCardBy IdentityMatcher ExtendedCardMatcher
  | BasicCardMatches CardMatcher
  | InDiscardOf IdentityMatcher ExtendedCardMatcher
  | TopmostCardInDiscardOf IdentityMatcher CardMatcher
  | ExtendedCardMatches [ExtendedCardMatcher]
  | NotCard PlayerCard
  | InHandOf IdentityMatcher ExtendedCardMatcher
  | -- | not a unique card whose title is already in play
    CanEnterPlay
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  x <> ExtendedCardMatches ys = ExtendedCardMatches $ x : ys
  ExtendedCardMatches xs <> y = ExtendedCardMatches $ xs <> [y]
  x <> y = ExtendedCardMatches [x, y]

data AttachmentMatcher = AttachmentWithId AttachmentId | AttachmentIs CardDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EnvironmentMatcher = AnyEnvironment | EnvironmentIs CardDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilityMatcher = AbilityWithType AbilityType | AbilityOnUpgrade UpgradeMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageMatcher
  = AttackFromPlayer IdentityMatcher
  | AttackFromAlly AllyMatcher
  | -- | damage a player dealt: by attacking, or with a card they control other than an
    -- ally
    DamageByPlayer IdentityMatcher
  | AnyDamage
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
