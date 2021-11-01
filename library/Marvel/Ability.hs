module Marvel.Ability where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Question
import Marvel.Source

data AbilityType = Response
  deriving stock Show

data Criteria = IsSelf
  deriving stock Show

data Limit = PerTurn Natural | PerRound Natural | NoLimit
  deriving stock Show

ability :: IsSource a => a -> Int -> Criteria -> Choice -> Ability
ability a = limitedAbility (toSource a) NoLimit

limitedAbility
  :: IsSource a => a -> Limit -> Int -> Criteria -> Choice -> Ability
limitedAbility a limit idx criteria choice = Ability
  { abilitySource = toSource a
  , abilityIndex = idx
  , abilityCriteria = criteria
  , abilityLimit = limit
  , abilityChoice = choice
  }

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityCriteria :: Criteria
  , abilityLimit :: Limit
  , abilityChoice :: Choice
  }
  deriving stock Show

class HasAbilities a where
  getAbilities :: a -> [Ability]
