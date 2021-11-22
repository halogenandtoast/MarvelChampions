module Marvel.Attack where

import Marvel.Prelude

import Marvel.Id

data Attack = Attack
  { attackEnemy :: EnemyId
  , attackDamage :: Natural
  , attackOverkill :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
