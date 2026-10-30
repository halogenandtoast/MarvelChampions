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
import Marvel.Ref (Source)
import Marvel.Status
import Marvel.Trait.Types

data EntityMatcher
  = IdentityEntity IdentityMatcher
  | AllyEntity AllyMatcher
  | MinionEntity MinionMatcher
  | AttachmentEntity AttachmentMatcher
  | -- | an event being played
    EventEntity EventId
  | MainSchemeEntity MainSchemeId
  | SideSchemeEntity SideSchemeMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data IdentityMatcher
  = HeroIdentity
  | AlterEgoIdentity
  | UnexhaustedIdentity
  | ExhaustedIdentity
  | ConfusedIdentity
  | StunnedIdentity
  | IdentityWithoutToughStatus
  | IdentityWithTitle Text
  | -- | the player playing as this identity, whichever face is up
    IdentityIs CardDef
  | IdentityWithId IdentityId
  | IdentityWithTrait Trait
  | AnyIdentity
  | IdentityWithPrintedHitPoints GameValueMatcher
  | IdentityWithDamage GameValueMatcher
  | -- | with so many counters on the identity card (Groot's growth counters)
    IdentityWithCounters GameValueMatcher
  | IdentityEngagedWith MinionMatcher
  | You
  | FirstPlayer
  | IdentityMatchAll [IdentityMatcher]
  | -- | not kept from thwarting (by a 'CannotThwart' modifier)
    IdentityCanThwart
  | -- | not kept from basic attacks (by a 'CannotBasicAttack' modifier, Seduced)
    IdentityCanBasicAttack
  | IdentityCanAttack
  | IdentityCanDefend
  | -- | not kept in its form (by a 'CannotChangeForm' modifier)
    IdentityCanChangeForm
  | -- | of the identities matching, those with the fewest hit points remaining
    IdentityWithFewestRemainingHitPoints IdentityMatcher
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
  | -- | the card in any of its printings
    AllyIs CardDef
  | AllyWithTitle Text
  | AllyWithPrintedCostAtLeast Natural
  | AllyWithThwartAtLeast Natural
  | AllyWithAttackAtLeast Natural
  | -- | allowed to be readied by a player-card effect (exhaustion checked separately)
    AllyCanReady
  | -- | its title or its subtitle the name: a named character (RR Team-Up)
    AllyNamedAs Text
  | -- | with exactly so many hit points remaining
    AllyWithRemainingHitPoints Natural
  | -- | with a trait its controller's identity has in its current form ("allies you
    -- control that share a trait with your hero", Mass Attack)
    AllySharingTraitWithController
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
  | SupportWithExhaustion Bool
  | SupportControlledBy IdentityMatcher
  | SupportWithUses GameValueMatcher
  | SupportNamed Name
  | SupportMatches [SupportMatcher]
  | SupportWithTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  = UpgradeCanBeReadied
  | UpgradeWithUses GameValueMatcher
  | UpgradeControlledBy IdentityMatcher
  | UpgradeWithTrait Trait
  | UpgradeWithKeyword Keyword
  | -- | A nonblanked upgrade whose Special can affect the game now.
    UpgradeWithResolvableSpecial
  | UpgradeIs CardDef
  | UpgradeNamed Name
  | UnexhaustedUpgrade
  | UpgradeWithId UpgradeId
  | UpgradeMatches [UpgradeMatcher]
  | UpgradeOneOf [UpgradeMatcher]
  | NotUpgrade UpgradeMatcher
  | -- | on an identity matching: controlled by it, and attached to nothing else
    UpgradeOnIdentity IdentityMatcher
  | UpgradeOnAlly AllyMatcher
  | -- | of the upgrades matching, those with the highest printed cost
    UpgradeWithHighestCostAmong UpgradeMatcher
  | -- | one an effect may make leave play: not a permanent one (RR, Permanent)
    UpgradeThatCanLeavePlay
  | UpgradeThatCanBeDiscarded
  | -- | an upgrade whose card matches
    UpgradeWithCard CardMatcher
  | -- | facedown: out of play for its text (Spectrum's energy forms)
    FacedownUpgrade
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
  | -- | a unique enemy: every villain, and a unique minion
    UniqueEnemy
  | -- | an enemy a player matching may attack: guard keeps the villain from them
    AttackableBy IdentityMatcher
  | -- | an enemy the ally may attack: as its controller may, and no minion for an ally
    -- that cannot attack minions (Drax)
    AttackableByAlly AllyId
  | EnemyCanTakeDamageFrom Source
  | DamageableEnemy
  | EnemyIs CardDef
  | NotEnemy EnemyMatcher
  | UndefendedEnemy
  | EnemyWithUpgrade UpgradeMatcher
  | EnemyWithAttachment AttachmentMatcher
  | EnemyMatches [EnemyMatcher]
  | DefeatedEnemy EnemyMatcher
  | -- | an enemy whose attack is under way
    AttackingEnemy
  | -- | an enemy whose attack under way is against a character matching, once defenders
    -- are declared
    EnemyAttackingCharacter CharacterMatcher
  | -- | the enemies whose ATK is the highest of all enemies'
    EnemyWithHighestAttack
  | StunnedEnemy
  | EnemyWithTrait Trait
  | ConfusedEnemy
  | -- | May receive a new confused status: neither stalwart nor already confused.
    EnemyCanBeConfused
  -- Galactic Artifacts: "attach to the enemy with the lowest ATK" (highest, lowest SCH)
  | EnemyWithLowestAttack
  | EnemyWithHighestScheme
  | EnemyWithLowestScheme
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

data SchemeMatcher
  = NotScheme SchemeMatcher
  | AnyScheme
  | MainScheme
  | EncounterMainScheme
  | ThwartableScheme
  | -- | no effect prohibits removing threat specifically by thwarting
    SchemeWithoutThwartPrevention
  | -- | threat can be removed from it, "ignoring any crisis icons in play"
    ThwartableSchemeIgnoringCrisis
  | -- | one "you" can thwart: a thwartable scheme, the main scheme not while a minion
    -- with patrol is engaged with you (RR Patrol)
    SchemeYouCanThwart
  | -- | as 'SchemeYouCanThwart', ignoring any crisis icons in play (Fandral's basic thwart)
    SchemeYouCanThwartIgnoringCrisis
  | -- | An ally uses its own keyword exceptions, not its controller's.
    SchemeAnAllyCanThwart AllyId Bool
  | SchemeAHeroCanBasicThwart IdentityId
  | -- | the main scheme, unless a patrol minion is engaged with you (RR Patrol)
    UnpatrolledMainScheme
  | SchemeWithId SchemeId
  | SchemeWithThreat GameValueMatcher
  | SchemeWithLeastThreat
  | SchemeMatches [SchemeMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup SchemeMatcher where
  AnyScheme <> x = x
  x <> AnyScheme = x
  SchemeMatches xs <> SchemeMatches ys = SchemeMatches (xs <> ys)
  SchemeMatches xs <> y = SchemeMatches (xs <> [y])
  x <> SchemeMatches ys = SchemeMatches (x : ys)
  x <> y = SchemeMatches [x, y]

instance Monoid SchemeMatcher where
  mempty = AnyScheme

data SideSchemeMatcher
  = AnySideScheme
  | -- | as it was, among those that have just left play too: what an "after it is
    -- defeated" window matches
    LeftPlaySideScheme SideSchemeMatcher
  | CrisisSideScheme
  | SideSchemeIs CardDef
  | SideSchemeWithId SideSchemeId
  | SideSchemeWithKeyword Keyword
  | SideSchemeWithUpgrade UpgradeMatcher
  | NotSideScheme SideSchemeMatcher
  | -- | the side scheme a villain matching owns, as Wrecker's Day of Reckoning
    SignatureSchemeOf VillainMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data MinionMatcher
  = AnyMinion
  | -- | as it was, among those that have just left play too: what an "after it is
    -- defeated" window matches
    LeftPlayMinion MinionMatcher
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
  | -- | of the minions matching, those with the most hit points remaining
    MinionWithMostRemainingHitPointsAmong MinionMatcher
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
  | -- | the identities of the players matching: "damages you"
    IdentityCharacters IdentityMatcher
  | CharacterWithTrait Trait
  | -- | an exhausted friendly character allowed to ready through a player card
    CharacterCanBeReadied
  | CharacterWithId CharacterId
  | CharacterOtherThan CharacterId
  | CharacterWithStatus StatusCard
  | -- | of the heroes and allies, those whose ATK is the highest
    HeroOrAllyWithHighestAttack
  | -- | of the heroes and of the allies an encounter card can attach to (not one that
    -- cannot have cards attached, as Odin), those with the highest ATK (Restrained)
    AttachableHeroOrAllyWithHighestAttack
  | -- | of the heroes and allies, those with the fewest hit points remaining
    HeroOrAllyWithFewestRemainingHitPoints
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
  | -- | a card the player could play ignoring its resource cost
    PlayableIgnoringCostBy IdentityMatcher ExtendedCardMatcher
  | BasicCardMatches CardMatcher
  | InSetAsideOf IdentityMatcher ExtendedCardMatcher
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

data AttachmentMatcher
  = AttachmentWithId AttachmentId
  | AttachmentOnEnemyWithHeroAbility EnemyId
  | AttachmentIs CardDef
  | AttachmentWithTrait Trait
  | -- | attached to the enemy
    AttachmentOnEnemy EnemyId
  | AttachmentMatches [AttachmentMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup AttachmentMatcher where
  AttachmentMatches xs <> AttachmentMatches ys = AttachmentMatches $ xs <> ys
  x <> AttachmentMatches ys = AttachmentMatches $ x : ys
  AttachmentMatches xs <> y = AttachmentMatches $ xs <> [y]
  x <> y = AttachmentMatches [x, y]

instance Monoid AttachmentMatcher where
  mempty = AttachmentMatches []

data EnvironmentMatcher
  = AnyEnvironment
  | EnvironmentIs CardDef
  | EnvironmentWithTitle Text
  | EnvironmentWithTrait Trait
  | -- | in the player's play area (Ebony Maw's Spells)
    EnvironmentInPlayAreaOf IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilityMatcher = AbilityWithType AbilityType | AbilityOnUpgrade UpgradeMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | What did something, by its source.
data SourceMatcher
  = AnySource
  | SourceIs Source
  | -- | the identity itself: its basic powers and its own card's abilities
    SourceIsIdentity IdentityMatcher
  | -- | an event a player matching played
    EventPlayedBy IdentityMatcher
  | SourceOneOf [SourceMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageMatcher
  = AttackFromPlayer IdentityMatcher
  | AttackFromAlly AllyMatcher
  | -- | damage a player dealt: by attacking, or with a card they control other than an
    -- ally
    DamageByPlayer IdentityMatcher
  | -- | damage dealt by the identity or a card extending it (RR You/Your)
    DamageByHero IdentityMatcher
  | AnyDamage
  | -- | an attack's, by a player or an ally
    AnyAttack
  | -- | "damage from an attack": an attack's, or what its overkill deals past a minion
    -- (RR Overkill; Master of Illusions)
    DamageFromAnAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
