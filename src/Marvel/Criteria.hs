module Marvel.Criteria where

import Marvel.Prelude
import Marvel.Phase (Phase)
import Marvel.Card.Code (CardCode)
import Marvel.Id (SupportId, UpgradeId)
import Marvel.Keyword (Keyword)

import Marvel.Ability.Types (AbilitySubType (..), AbilityType (..))
import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import Marvel.Matchers.Types
import Marvel.Resource.Types
import Marvel.Trait.Types (Trait)

data Criteria
  = IsSelf
  | CanFindTouched
  | -- | the acting player can resolve the current unblanked Setting Special as a cost
    CanResolveSettingSpecial
  | NemesisMinionOutOfPlay
  | NoCriteria
  | InHeroForm
  | InAlterEgoForm
  | Unexhausted
  | Exhausted
  | OwnsThis
  | DuringPhase Phase
  | NoCardsPlayedThisPhase
  | HeldWeaponSwapAvailable UpgradeId
  | EncounterIconExists
  | PoolIconEffectsCanResolve
  | PoolAllyInHandCanEnterPlay
  | MovableThreatExists
  | PlayedEventInDiscard
  | MinionExists MinionMatcher
  | EnemyExists EnemyMatcher
  | SchemeExists SchemeMatcher
  | AllyExists AllyMatcher
  | AttachmentExists AttachmentMatcher
  | SideSchemeExists SideSchemeMatcher
  | SupportExists SupportMatcher
  | HeldAllyPlayable SupportId
  | MedLabCaptureAvailable
  | CharacterExists CharacterMatcher
  | SelfMatches IdentityMatcher
  | ExtendedCardExists ExtendedCardMatcher
  | EncounterDiscardHas CardMatcher
  | IdentityAttackedAndThwartedThisPhase IdentityMatcher
  | Never
  | Criteria [Criteria]
  | -- | any one of them
    AnyCriteria [Criteria]
  | -- | The enclosed condition must not hold.
    NotCriteria Criteria
  | UpgradeExists UpgradeMatcher
  | EnvironmentExists EnvironmentMatcher
  | -- | a resource ability that pays only for a card matching, as "for an event"
    PayingFor CardMatcher
  | PayingForAbility CardCode Natural
  | SourceHasKeyword Keyword
  | -- | the card's resource cost is paid with only this type of resource: "you can
    -- only spend [physical] resources to pay for this card"
    SpendOnly Resource
  | -- | a resource ability its controller resolves for any player paying: "generate a
    -- [wild] resource for any player" (the Milano); any player may use it, the card's
    -- controller deemed to agree, unless peril keeps the controller from helping
    ForAnyPlayer
  | -- | there is a card in The Collection (Infiltrate the Museum)
    CollectionHasCards
  | VictoryDisplayHasSideScheme
  | -- | another card remains playable after this card exhausts the identity
    CardPlayAfterExhaustingHasTarget Natural
  | PlayCardWithReductionHasTarget CardMatcher Natural
  | CanPlayAttackEventWithClaws
  | ControlsCardsWithTrait Trait Natural
  | CanCancelRevealedEncounterCard
  | CanCancelRevealedTreachery
  | CanResolvePrintedHeroAbilities
  | PendingDamageAtLeast Natural
  | HasOtherSchemeForThreatMovement
  | HasOtherSchemeForThreatRemoval
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Monoid Criteria where
  mempty = NoCriteria

instance Semigroup Criteria where
  NoCriteria <> x = x
  x <> NoCriteria = x
  Criteria xs <> Criteria ys = Criteria $ xs <> ys
  Criteria xs <> y = Criteria $ xs <> [y]
  x <> Criteria ys = Criteria $ x : ys
  x <> y = Criteria [x, y]

-- | The cards a resource ability may pay for, if it is limited to some.
payingForMatchers :: Criteria -> [CardMatcher]
payingForMatchers = \case
  PayingFor m -> [m]
  Criteria cs -> concatMap payingForMatchers cs
  _ -> []

-- | Whether any player may have the ability resolved for them ('ForAnyPlayer').
forAnyPlayer :: Criteria -> Bool
forAnyPlayer = \case
  ForAnyPlayer -> True
  Criteria cs -> any forAnyPlayer cs
  _ -> False

-- | The form a "Hero …" or "Alter-Ego …" ability is used in.
abilityTypeCriteria :: AbilityType -> Criteria
abilityTypeCriteria = \case
  AlterEgoInterrupt -> InAlterEgoForm
  HeroInterrupt -> InHeroForm
  HeroResource -> InHeroForm
  AlterEgoResponse -> InAlterEgoForm
  HeroResponse -> InHeroForm
  HeroAction -> InHeroForm
  AlterEgoAction -> InAlterEgoForm
  AlterEgoResource -> InAlterEgoForm
  _ -> NoCriteria

-- | The type of resource a card's cost must be paid with, if only one may be.
spendOnly :: Criteria -> Maybe Resource
spendOnly = \case
  SpendOnly r -> Just r
  Criteria xs -> headMay (mapMaybe spendOnly xs)
  _ -> Nothing

-- | Whether a resource may be spent on a card with these criteria: a wild resource is
-- spent as any type.
canSpendOn :: Criteria -> Resource -> Bool
canSpendOn criteria r = maybe True (\only -> r == only || r == Wild) (spendOnly criteria)
-- | What an attack needs: an enemy to attack, or a stun for the attempt to remove. A
-- stunned character can attempt an attack with no target ("Stunned").
canAttack :: Criteria
canAttack = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy]

-- | What an attack on the villain needs: one that can be attacked, or a stun for the
-- attempt to remove.
canAttackVillain :: Criteria
canAttackVillain = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists (VillainEnemy <> AttackableEnemy)]

-- | What an attack on a minion needs: a minion, or a stun for the attempt to remove.
canAttackMinion :: Criteria
canAttackMinion = AnyCriteria [SelfMatches StunnedIdentity, MinionExists AnyMinion]

-- | What a thwart needs of the player making it.
abilitySubTypeCriteria :: AbilitySubType -> Criteria
abilitySubTypeCriteria = \case
  Thwart -> SelfMatches IdentityCanThwart
  Attack -> SelfMatches IdentityCanAttack
  Defense -> SelfMatches IdentityCanDefend
