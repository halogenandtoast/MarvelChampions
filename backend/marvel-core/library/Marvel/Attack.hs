module Marvel.Attack where

import Marvel.Prelude

import Marvel.Id
import Marvel.Ref

data Attack = Attack
  { attackCharacter :: CharacterId
  , attackOverkill :: Bool
  , attackDamage :: Natural
  , attackEnemy :: EnemyId
  , attackDefended :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

attackSource :: Attack -> Source
attackSource a = case attackEnemy a of
  EnemyMinionId mid -> MinionRef mid
  EnemyVillainId vid -> VillainRef vid

attackCharacterL :: Lens' Attack CharacterId
attackCharacterL = lens attackCharacter $ \m x -> m {attackCharacter = x}

attackOverkillL :: Lens' Attack Bool
attackOverkillL = lens attackOverkill $ \m x -> m {attackOverkill = x}

attackDamageL :: Lens' Attack Natural
attackDamageL = lens attackDamage $ \m x -> m {attackDamage = x}

attack :: EnemyId -> CharacterId -> Natural -> Attack
attack enemyId characterId dmg =
  Attack
    { attackCharacter = characterId
    , attackOverkill = False
    , attackDamage = dmg
    , attackEnemy = enemyId
    , attackDefended = False
    }
