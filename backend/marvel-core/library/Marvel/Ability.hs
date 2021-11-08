module Marvel.Ability
  ( module Marvel.Ability
  , module X
  ) where

import Marvel.Prelude

import qualified Data.HashMap.Strict as HashMap
import Marvel.Ability.Type as X
import Marvel.Game.Source
import Marvel.Id
import {-# SOURCE #-} Marvel.Question
import Marvel.Source
import Marvel.Window

ability
  :: IsSource a => a -> Natural -> AbilityType -> Criteria -> Choice -> Ability
ability a idx aType =
  limitedAbility (toSource a) idx (defaultAbilityLimit aType) aType

windowAbility
  :: IsSource a
  => a
  -> Natural
  -> WindowMatcher
  -> AbilityType
  -> Choice
  -> Ability
windowAbility a idx window aType choice =
  limitedAbility
      (toSource a)
      idx
      (defaultAbilityLimit aType)
      aType
      NoCriteria
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
  -> Choice
  -> Ability
limitedAbility a idx limit aType criteria choice = Ability
  { abilitySource = toSource a
  , abilityIndex = idx
  , abilityCriteria = criteria
  , abilityLimit = limit
  , abilityTiming = pure DuringOwnTurn
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

passesCriteria :: IdentityId -> Ability -> Bool
passesCriteria x a = case abilityCriteria a of
  IsSelf -> IdentitySource x == abilitySource a
  NoCriteria -> True

passesTiming :: IdentityId -> Ability -> Bool
passesTiming _ a = all passes (toList $ abilityTiming a)
 where
  passes x = case x of
    DuringOwnTurn -> True

passesTypeIsRelevant :: IdentityId -> Ability -> Bool
passesTypeIsRelevant _ a = case abilityType a of
  Interrupt -> False
  HeroInterrupt -> False
  ForcedInterrupt -> False
  Resource -> False
  HeroResource -> False
  Response -> False
  ForcedResponse -> False
  Action -> True
  HeroAction -> False
  AlterEgoAction -> False
  Special -> False

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
