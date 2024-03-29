module Marvel.Ability (
  module Marvel.Ability,
  module X,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types as X
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window

ability ::
  (IsRef a) =>
  a ->
  Natural ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
ability a idx aType =
  limitedAbility (toSource a) idx (defaultAbilityLimit aType) aType

windowAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  WindowMatcher ->
  AbilityType ->
  Cost ->
  Choice ->
  Ability
windowAbility a idx window aType cost choice =
  limitedAbility
    (toSource a)
    idx
    (defaultAbilityLimit aType)
    aType
    NoCriteria
    cost
    choice
    & windowL
      ?~ window

limitedWindowAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  WindowMatcher ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
limitedWindowAbility a idx window aType criteria cost choice =
  limitedAbility
    (toSource a)
    idx
    (defaultAbilityLimit aType)
    aType
    criteria
    cost
    choice
    & windowL
      ?~ window

limitedAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  Limit ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
limitedAbility a idx limit aType criteria cost choice =
  Ability
    { abilitySource = toSource a
    , abilityIndex = idx
    , abilityCriteria = criteria
    , abilityLimit = limit
    , abilityTiming = pure DuringOwnTurn
    , abilityCost = cost
    , abilityChoices = [choice]
    , abilityType = aType
    , abilitySubType = Nothing
    , abilityLabel = Nothing
    , abilityWindow = Nothing
    }

label :: Text -> Ability -> Ability
label l a = a {abilityLabel = Just l}

limited :: Limit -> Ability -> Ability
limited l a = a {abilityLimit = l}

subtype :: AbilitySubType -> Ability -> Ability
subtype s a = a {abilitySubType = Just s}

passesUseLimit :: IdentityId -> HashMap IdentityId [Ability] -> Ability -> Bool
passesUseLimit x aMap a = case abilityLimit a of
  NoLimit -> True
  PerTurn n -> count (== a) usedAbilities < n
  PerRound n -> count (== a) usedAbilities < n
  PerWindow n -> count (== a) usedAbilities < n
 where
  usedAbilities = HashMap.findWithDefault [] x aMap

passesCriteria :: (Projection m PlayerIdentity, MonadThrow m, HasGame m, HasQueue m) => IdentityId -> Ability -> m Bool
passesCriteria x a = go (abilityCriteria a)
 where
  go = \case
    IsSelf -> pure $ toSource x == source
    SelfMatches identityMatcher ->
      member x <$> select (IdentityWithId x <> identityMatcher)
    NoCriteria -> pure True
    Never -> pure False
    InHeroForm -> member x <$> select HeroIdentity
    InAlterEgoForm -> member x <$> select AlterEgoIdentity
    Exhausted -> member x <$> select ExhaustedIdentity
    Unexhausted -> member x <$> select UnexhaustedIdentity
    OwnsThis -> case abilitySource a of
      AllyRef aid ->
        member aid <$> select (AllyControlledBy $ IdentityWithId x)
      SupportRef aid ->
        member aid <$> select (SupportControlledBy $ IdentityWithId x)
      UpgradeRef aid ->
        member aid <$> select (UpgradeControlledBy $ IdentityWithId x)
      _ -> error $ "Unhandled " <> show (abilitySource a)
    Criteria xs -> allM go xs
    MinionExists m -> selectAny m
    CharacterExists m -> selectAny m
    EnemyExists m -> selectAny m
    SchemeExists m -> selectAny m
    AllyExists m -> selectAny m
    ExtendedCardExists m -> selectAny m
  source = abilitySource a

partitionMaybes :: [Maybe a] -> ([()], [a])
partitionMaybes = foldr (maybe nothing just) ([], [])
 where
  nothing ~(l, r) = (() : l, r)
  just a ~(l, r) = (l, a : r)

passesTiming :: IdentityId -> Ability -> Bool
passesTiming _ a = all passes (toList $ abilityTiming a)
 where
  passes x = case x of
    DuringOwnTurn -> True

passesTypeIsRelevant :: (HasGame m) => IdentityId -> Ability -> m Bool
passesTypeIsRelevant ident a = case abilityType a of
  Interrupt -> pure False
  HeroInterrupt -> pure False
  ForcedInterrupt -> pure False
  Resource -> pure False
  HeroResource -> pure False
  Response -> pure False
  ForcedResponse -> pure False
  Action -> pure True
  Basic -> pure True
  HeroAction -> member ident <$> select HeroIdentity
  AlterEgoAction -> member ident <$> select AlterEgoIdentity
  Special -> pure False
  Setup -> pure False

class PerformAbility a where
  performAbility :: (HasGame m) => a -> Natural -> m ()

isForcedAbility :: Ability -> Bool
isForcedAbility a = case abilityType a of
  Interrupt -> False
  HeroInterrupt -> False
  ForcedInterrupt -> True
  Resource -> False
  HeroResource -> False
  Response -> False
  ForcedResponse -> True
  Action -> False
  HeroAction -> False
  AlterEgoAction -> False
  Special -> False
  Basic -> False
  Setup -> False

defaultAbilityLimit :: AbilityType -> Limit
defaultAbilityLimit = \case
  Interrupt -> PerWindow 1
  HeroInterrupt -> PerWindow 1
  ForcedInterrupt -> PerWindow 1
  Resource -> NoLimit
  HeroResource -> NoLimit
  Response -> PerWindow 1
  ForcedResponse -> PerWindow 1
  Action -> NoLimit
  HeroAction -> NoLimit
  AlterEgoAction -> NoLimit
  Special -> NoLimit
  Basic -> NoLimit
  Setup -> NoLimit
