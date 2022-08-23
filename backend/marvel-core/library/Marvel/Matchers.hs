{-# LANGUAGE PatternSynonyms #-}

module Marvel.Matchers where

import Marvel.Prelude

import Marvel.Ability.Type
import {-# SOURCE #-} Marvel.Card.Def
import {-# SOURCE #-} Marvel.Card.EncounterCard
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Trait

data EntityMatcher
  = IdentityEntity IdentityMatcher
  | AllyEntity AllyMatcher
  | AttachmentEntity AttachmentMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data IdentityMatcher
  = HeroIdentity
  | AlterEgoIdentity
  | UnexhaustedIdentity
  | ExhaustedIdentity
  | ConfusedIdentity
  | StunnedIdentity
  | IdentityWithId IdentityId
  | IdentityWithTrait Trait
  | AnyIdentity
  | IdentityWithDamage GameValueMatcher
  | IdentityEngagedWith MinionMatcher
  | You
  | FirstPlayer
  | IdentityMatchAll [IdentityMatcher]
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

identityMatches :: MonadGame env m => IdentityMatcher -> IdentityId -> m Bool
identityMatches matcher ident = member ident <$> gameSelectIdentity matcher

pattern IdentityWithAnyDamage :: IdentityMatcher
pattern IdentityWithAnyDamage <-
  IdentityWithDamage (GreaterThan (Static 0)) where
  IdentityWithAnyDamage = IdentityWithDamage (GreaterThan (Static 0))

instance Count IdentityMatcher where
  data QueryCount IdentityMatcher = SustainedDamage | HeroAttackDamage
  selectCount = gameSelectCountIdentity

data AllyMatcher
  = UnexhaustedAlly
  | ExhaustedAlly
  | AllyWithUses GameValueMatcher
  | AllyControlledBy IdentityMatcher
  | AllyWithDamage GameValueMatcher
  | AllyWithId AllyId
  | AnyAlly
  | AllyMatches [AllyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AllyWithAnyUses :: AllyMatcher
pattern AllyWithAnyUses <- AllyWithUses (GreaterThan (Static 0)) where
  AllyWithAnyUses = AllyWithUses (GreaterThan (Static 0))

allyMatches :: MonadGame env m => AllyMatcher -> AllyId -> m Bool
allyMatches matcher ident = member ident <$> gameSelectAlly matcher

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

pattern SupportWithAnyUses :: SupportMatcher
pattern SupportWithAnyUses <- SupportWithUses (GreaterThan (Static 0)) where
  SupportWithAnyUses = SupportWithUses (GreaterThan (Static 0))

pattern UpgradeWithAnyUses :: UpgradeMatcher
pattern UpgradeWithAnyUses <- UpgradeWithUses (GreaterThan (Static 0)) where
  UpgradeWithAnyUses = UpgradeWithUses (GreaterThan (Static 0))

data UpgradeMatcher
  = UpgradeWithUses GameValueMatcher
  | UpgradeControlledBy IdentityMatcher
  | UpgradeWithTrait Trait
  | UnexhaustedUpgrade
  | UpgradeMatches [UpgradeMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup UpgradeMatcher where
  UpgradeMatches xs <> UpgradeMatches ys = UpgradeMatches $ xs <> ys
  x <> UpgradeMatches ys = UpgradeMatches $ x : ys
  UpgradeMatches xs <> y = UpgradeMatches $ xs <> [y]
  x <> y = UpgradeMatches [x, y]

upgradeMatches :: MonadGame env m => UpgradeMatcher -> UpgradeId -> m Bool
upgradeMatches matcher ident = member ident <$> gameSelectUpgrade matcher

data EnemyMatcher
  = AnyEnemy
  | EnemyWithId EnemyId
  | VillainEnemy
  | MinionEnemy
  | AttackableEnemy
  | DamageableEnemy
  | EnemyIs CardDef
  | NotEnemy EnemyMatcher
  | UndefendedEnemy
  | EnemyMatchesAll [EnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatchesAll xs <> EnemyMatchesAll ys = EnemyMatchesAll (xs <> ys)
  EnemyMatchesAll xs <> y = EnemyMatchesAll (xs <> [y])
  x <> EnemyMatchesAll ys = EnemyMatchesAll (x : ys)
  x <> y = EnemyMatchesAll [x, y]

enemyMatches :: MonadGame env m => EnemyMatcher -> EnemyId -> m Bool
enemyMatches matcher ident = member ident <$> gameSelectEnemy matcher

pattern VillainWithAnyDamage :: VillainMatcher
pattern VillainWithAnyDamage <-
  VillainWithDamage (GreaterThan (Static 0)) where
  VillainWithAnyDamage = VillainWithDamage (GreaterThan (Static 0))

data VillainMatcher
  = ActiveVillain
  | AnyVillain
  | VillainWithId VillainId
  | VillainWithDamage GameValueMatcher
  | VillainWithToughStatus
  | VillainMatches [VillainMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup VillainMatcher where
  VillainMatches xs <> VillainMatches ys = VillainMatches $ xs <> ys
  x <> VillainMatches ys = VillainMatches $ x : ys
  VillainMatches xs <> y = VillainMatches $ xs <> [y]
  x <> y = VillainMatches [x, y]

villainMatches :: MonadGame env m => VillainMatcher -> VillainId -> m Bool
villainMatches matcher ident = member ident <$> gameSelectVillain matcher

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

data SchemeMatcher = AnyScheme | MainScheme | ThwartableScheme | SchemeWithId SchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

schemeMatches :: MonadGame env m => SchemeMatcher -> SchemeId -> m Bool
schemeMatches matcher ident = member ident <$> gameSelectScheme matcher

instance Count SchemeMatcher where
  data QueryCount SchemeMatcher = SchemeThreat
  selectCount = gameSelectCountScheme

data SideSchemeMatcher
  = AnySideScheme
  | CrisisSideScheme
  | SideSchemeIs CardDef
  | SideSchemeWithId SideSchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

sideSchemeMatches
  :: MonadGame env m => SideSchemeMatcher -> SideSchemeId -> m Bool
sideSchemeMatches matcher ident = member ident <$> gameSelectSideScheme matcher

data MinionMatcher
  = AnyMinion
  | MinionWithId MinionId
  | MinionWithDamage GameValueMatcher
  | MinionWithTrait Trait
  | MinionWithKeyword Keyword
  | MinionEngagedWith IdentityMatcher
  | MinionIs CardDef
  | MinionWithHighestPrintedHitPoints
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

minionMatches :: MonadGame env m => MinionMatcher -> MinionId -> m Bool
minionMatches matcher ident = member ident <$> gameSelectMinion matcher

data GameValueMatcher = AnyValue | GreaterThan GameValue | AtLeast GameValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

gameValueMatches :: MonadGame env m => GameValueMatcher -> Natural -> m Bool
gameValueMatches matcher n = case matcher of
  AnyValue -> pure True
  GreaterThan v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n > value
  AtLeast v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n >= value

pattern CharacterWithAnyDamage :: CharacterMatcher
pattern CharacterWithAnyDamage <-
  CharacterWithDamage (GreaterThan (Static 0)) where
  CharacterWithAnyDamage = CharacterWithDamage (GreaterThan (Static 0))

data CharacterMatcher
  = CharacterWithDamage GameValueMatcher
  | DamageableCharacter
  | AnyCharacter
  | CharacterMatches [CharacterMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CharacterMatcher where
  AnyCharacter <> x = x
  x <> AnyCharacter = x
  CharacterMatches xs <> CharacterMatches ys = CharacterMatches $ xs <> ys
  x <> CharacterMatches ys = CharacterMatches $ x : ys
  CharacterMatches xs <> y = CharacterMatches $ xs <> [y]
  x <> y = CharacterMatches [x, y]

characterMatches
  :: MonadGame env m => CharacterMatcher -> CharacterId -> m Bool
characterMatches matcher ident = member ident <$> gameSelectCharacter matcher

newtype EncounterCardMatcher = NemesisSetFor IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data BasicEncounterCardMatcher = AnyEncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

encounterCardMatches :: BasicEncounterCardMatcher -> EncounterCard -> Bool
encounterCardMatches matcher _ = case matcher of
  AnyEncounterCard -> True

data ExtendedCardMatcher
  = AffordableCardBy IdentityMatcher ExtendedCardMatcher
  | BasicCardMatches CardMatcher
  | InDiscardOf IdentityMatcher ExtendedCardMatcher
  | TopmostCardInDiscardOf IdentityMatcher CardMatcher
  | ExtendedCardMatches [ExtendedCardMatcher]
  | NotCard PlayerCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  x <> ExtendedCardMatches ys = ExtendedCardMatches $ x : ys
  ExtendedCardMatches xs <> y = ExtendedCardMatches $ xs <> [y]
  x <> y = ExtendedCardMatches [x, y]

newtype AttachmentMatcher = AttachmentWithId AttachmentId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilityMatcher = AbilityWithType AbilityType | AbilityOnUpgrade UpgradeMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageMatcher = AttackFromPlayer IdentityMatcher | AttackFromAlly AllyMatcher | AnyDamage
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

damageMatches :: MonadGame env m => DamageMatcher -> Damage -> m Bool
damageMatches matcher damage = case matcher of
  AttackFromPlayer identityMatcher -> case damageSource damage of
    FromPlayerAttack ident -> identityMatches identityMatcher ident
    _ -> pure False
  AttackFromAlly allyMatcher -> case damageSource damage of
    FromAllyAttack ident -> allyMatches allyMatcher ident
    _ -> pure False
  AnyDamage -> pure True
