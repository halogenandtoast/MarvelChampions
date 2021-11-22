module Marvel.Attack where

import Marvel.Prelude

import Marvel.Id
import Marvel.Source

data Attack = Attack
  { attackCharacter :: CharacterId
  , attackOverkill :: Bool
  , attackDamage :: Natural
  , attackSource :: Source
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

attackCharacterL :: Lens' Attack CharacterId
attackCharacterL = lens attackCharacter $ \m x -> m { attackCharacter = x }

attackOverkillL :: Lens' Attack Bool
attackOverkillL = lens attackOverkill $ \m x -> m { attackOverkill = x }

attackDamageL :: Lens' Attack Natural
attackDamageL = lens attackDamage $ \m x -> m { attackDamage = x }

attack :: IsSource a => a -> CharacterId -> Natural -> Attack
attack a characterId dmg = Attack
  { attackCharacter = characterId
  , attackOverkill = False
  , attackDamage = dmg
  , attackSource = toSource a
  }
