module Marvel.Ability.Type where

import Marvel.Prelude

import GHC.Generics
import {-# SOURCE #-} Marvel.Question
import Marvel.Source

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
  -- | WhenRevealed
  deriving stock (Show, Eq)

data AbilitySubType = Attack | Defense | Thwart
  deriving stock (Show, Eq)

data Criteria = IsSelf
  deriving stock (Show, Eq)

data Limit = PerTurn Natural | PerRound Natural | NoLimit
  deriving stock (Show, Eq)

data AbilityTiming = DuringOwnTurn
  deriving stock (Show, Eq)

data Ability = Ability
  { abilitySource :: Source
  , abilityCriteria :: Criteria
  , abilityLimit :: Limit
  , abilityTiming :: NonEmpty AbilityTiming
  , abilityChoice :: Choice
  , abilityType :: AbilityType
  , abilitySubType :: Maybe AbilitySubType
  , abilityLabel :: Maybe Text
  }
  deriving stock (Show, Eq)

class HasAbilities a where
  getAbilities :: a -> [Ability]

genericGetAbilities :: (Generic a, HasAbilities' (Rep a)) => a -> [Ability]
genericGetAbilities = getAbilities' . from

class HasAbilities' f where
  getAbilities' :: f p -> [Ability]

instance HasAbilities' f => HasAbilities' (M1 i c f) where
  getAbilities' = getAbilities' . unM1

instance (HasAbilities' l, HasAbilities' r) => HasAbilities' (l :+: r) where
  getAbilities' = \case
    L1 l -> getAbilities' l
    R1 r -> getAbilities' r

instance HasAbilities c => HasAbilities' (K1 i c) where
  getAbilities' = getAbilities . unK1
