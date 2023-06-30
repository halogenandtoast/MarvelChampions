module Marvel.Ability.Types where

import Marvel.Prelude

import Marvel.Cost.Types
import {-# SOURCE #-} Marvel.Criteria
import {-# SOURCE #-} Marvel.Question
import Marvel.Ref
import {-# SOURCE #-} Marvel.Window.Types

data AbilityType
  = Interrupt
  | HeroInterrupt
  | ForcedInterrupt
  | Resource
  | HeroResource
  | Response
  | ForcedResponse
  | Action
  | HeroAction
  | AlterEgoAction
  | Special
  | Basic
  | Setup
  -- \| WhenRevealed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilitySubType
  = Attack
  | Defense
  | Thwart
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Limit
  = PerTurn Natural
  | PerRound Natural
  | PerWindow Natural
  | NoLimit
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AbilityTiming = DuringOwnTurn
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Natural
  , abilityCriteria :: Criteria
  , abilityLimit :: Limit
  , abilityTiming :: NonEmpty AbilityTiming
  , abilityChoices :: [Choice]
  , abilityType :: AbilityType
  , abilitySubType :: Maybe AbilitySubType
  , abilityLabel :: Maybe Text
  , abilityWindow :: Maybe WindowMatcher
  , abilityCost :: Cost
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Are two abilities equal?
-- Hack for handling two sides of a card as long as abilities have
-- different labels
instance Eq Ability where
  a == b =
    (abilitySource a == abilitySource b)
      && (abilityIndex a == abilityIndex b)
      && (abilityLabel a == abilityLabel b)

windowL :: Lens' Ability (Maybe WindowMatcher)
windowL = lens abilityWindow $ \m x -> m {abilityWindow = x}

choicesL :: Lens' Ability [Choice]
choicesL = lens abilityChoices $ \m x -> m {abilityChoices = x}

class HasAbilities a where
  getAbilities :: (HasCallStack) => a -> [Ability]
  getAbilities _ = []
