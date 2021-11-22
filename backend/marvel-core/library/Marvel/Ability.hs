module Marvel.Ability
  ( module Marvel.Ability
  , module X
  ) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Type as X
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import {-# SOURCE #-} Marvel.Question
import Marvel.Source
import Marvel.Window

ability
  :: IsSource a
  => a
  -> Natural
  -> AbilityType
  -> Criteria
  -> Cost
  -> Choice
  -> Ability
ability a idx aType =
  limitedAbility (toSource a) idx (defaultAbilityLimit aType) aType

windowAbility
  :: IsSource a
  => a
  -> Natural
  -> WindowMatcher
  -> AbilityType
  -> Cost
  -> Choice
  -> Ability
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

limitedWindowAbility
  :: IsSource a
  => a
  -> Natural
  -> WindowMatcher
  -> AbilityType
  -> Criteria
  -> Cost
  -> Choice
  -> Ability
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

limitedAbility
  :: IsSource a
  => a
  -> Natural
  -> Limit
  -> AbilityType
  -> Criteria
  -> Cost
  -> Choice
  -> Ability
limitedAbility a idx limit aType criteria cost choice = Ability
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
label l a = a { abilityLabel = Just l }

passesUseLimit :: IdentityId -> HashMap IdentityId [Ability] -> Ability -> Bool
passesUseLimit x aMap a = case abilityLimit a of
  NoLimit -> True
  PerTurn n -> count (== a) usedAbilities < n
  PerRound n -> count (== a) usedAbilities < n
  PerWindow n -> count (== a) usedAbilities < n
  where usedAbilities = HashMap.findWithDefault [] x aMap

passesCriteria :: MonadGame env m => IdentityId -> Ability -> m Bool
passesCriteria x a = go (abilityCriteria a)
 where
  go = \case
    IsSelf -> pure $ toSource x == source
    SelfMatches identityMatcher ->
      member x <$> select (IdentityWithId x <> identityMatcher)
    NoCriteria -> pure True
    Never -> pure False
    InHeroForm -> member x <$> select HeroIdentity
    Unexhausted -> member x <$> select UnexhaustedIdentity
    OwnsThis -> case abilitySource a of
      AllySource aid ->
        member aid <$> select (AllyControlledBy $ IdentityWithId x)
      SupportSource aid ->
        member aid <$> select (SupportControlledBy $ IdentityWithId x)
      UpgradeSource aid ->
        member aid <$> select (UpgradeControlledBy $ IdentityWithId x)
      _ -> error $ "Unhandled " <> show (abilitySource a)
    Criteria xs -> allM go xs
    MinionExists m -> selectAny m
    CharacterExists m -> selectAny m
    AllyExists m -> selectAny m
    ExtendedCardExists m -> selectAny m
  source = abilitySource a

passesCanAffordCost :: MonadGame env m => IdentityId -> Ability -> m Bool
passesCanAffordCost _ a = go (abilityCost a)
 where
  go = \case
    NoCost -> pure True
    ExhaustCost -> case source of
      IdentitySource ident -> member ident <$> select UnexhaustedIdentity
      AllySource ident -> member ident <$> select UnexhaustedAlly
      SupportSource ident -> member ident <$> select UnexhaustedSupport
      UpgradeSource ident -> member ident <$> select UnexhaustedUpgrade
      _ -> error "Unhandled"
    UseCost -> case source of
      UpgradeSource ident -> member ident <$> select UpgradeWithAnyUses
      AllySource ident -> member ident <$> select AllyWithAnyUses
      _ -> error "Unhandled"
    ResourceCost mr -> do
      resources <- getAvailableResourcesFor Nothing
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources
    Costs xs -> allM go xs
  source = abilitySource a

passesTiming :: IdentityId -> Ability -> Bool
passesTiming _ a = all passes (toList $ abilityTiming a)
 where
  passes x = case x of
    DuringOwnTurn -> True

passesTypeIsRelevant :: MonadGame env m => IdentityId -> Ability -> m Bool
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

class PerformAbility a where
  performAbility :: MonadGame env m => a -> Natural -> m ()

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
