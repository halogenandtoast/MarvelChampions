module Marvel.Ability.Types where

import Marvel.Prelude

import Marvel.Cost.Types
import {-# SOURCE #-} Marvel.Criteria
import {-# SOURCE #-} Marvel.Question
import Marvel.Ref
import {-# SOURCE #-} Marvel.Window.Types

data AbilityType
  = Interrupt
  | AlterEgoInterrupt
  | HeroInterrupt
  | ForcedInterrupt
  | Resource
  | HeroResource
  | AlterEgoResource
  | Response
  | AlterEgoResponse
  | HeroResponse
  | ForcedResponse
  | Action
  | HeroAction
  | AlterEgoAction
  | Special
  | Basic
  | Setup
  | -- | a "When Revealed" ability: forced, resolved as its card is revealed
    WhenRevealed
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
  | -- | A maximum shared by copies of a named ability during this occurrence.
    SharedPerWindow Text Natural
  | NoLimit
  | PerPhase Natural
  | -- | "(Limit ... per player.)": each player has the limit of their own, on a card
    -- anyone may trigger
    EachPlayer Limit
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A limit's period and count, whether it is each player's or the card's.
baseLimit :: Limit -> Limit
baseLimit = \case
  EachPlayer l -> baseLimit l
  l -> l

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
