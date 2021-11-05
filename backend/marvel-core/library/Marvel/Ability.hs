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

ability :: IsSource a => a -> AbilityType -> Criteria -> Choice -> Ability
ability a = limitedAbility (toSource a) NoLimit

limitedAbility
  :: IsSource a => a -> Limit -> AbilityType -> Criteria -> Choice -> Ability
limitedAbility a limit aType criteria choice = Ability
  { abilitySource = toSource a
  , abilityCriteria = criteria
  , abilityLimit = limit
  , abilityTiming = pure DuringOwnTurn
  , abilityChoice = choice
  , abilityType = aType
  , abilitySubType = Nothing
  , abilityLabel = Nothing
  }

label :: Text -> Ability -> Ability
label l a = a { abilityLabel = Just l }

passesUseLimit :: IdentityId -> HashMap IdentityId [Ability] -> Ability -> Bool
passesUseLimit x aMap a = case abilityLimit a of
  NoLimit -> True
  PerTurn n -> count (== a) usedAbilities < n
  PerRound n -> count (== a) usedAbilities < n
  where usedAbilities = HashMap.findWithDefault [] x aMap

passesCriteria :: IdentityId -> Ability -> Bool
passesCriteria x a = case abilityCriteria a of
  IsSelf -> IdentitySource x == abilitySource a

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
