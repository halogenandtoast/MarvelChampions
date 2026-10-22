module Marvel.Attack where

import Marvel.Prelude

import Marvel.Damage
import Marvel.Id
import Marvel.Keyword
import Marvel.Ref

data Attack = Attack
  { attackCharacter :: CharacterId
  , attackOverkill :: Bool
  , attackDamage :: Natural
  , attackEnemy :: EnemyId
  , attackDefended :: Bool
  , attackPiercing :: Bool
  , attackRanged :: Bool
  , attackPrevented :: Bool
  -- ^ "prevent all damage from this attack"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

attackSource :: Attack -> Source
attackSource a = case attackEnemy a of
  EnemyMinionId mid -> MinionRef mid
  EnemyVillainId vid -> VillainRef vid

-- | The enemy a source names, as the source of an enemy's attack does.
sourceEnemy :: Source -> Maybe EnemyId
sourceEnemy = \case
  MinionRef mid -> Just (EnemyMinionId mid)
  VillainRef vid -> Just (EnemyVillainId vid)
  EnemyRef eid -> Just eid
  _ -> Nothing

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
    , attackPiercing = False
    , attackRanged = False
    , attackPrevented = False
    }

-- | The damage the attack deals: none once all of it is prevented.
attackDamageDealt :: Attack -> Natural
attackDamageDealt a = if attackPrevented a then 0 else attackDamage a

-- | "The attack gains overkill, piercing or ranged"; no other keyword is an attack's.
gainKeyword :: Keyword -> Attack -> Attack
gainKeyword k a = case k of
  Overkill -> a {attackOverkill = True}
  Piercing -> a {attackPiercing = True}
  Ranged -> a {attackRanged = True}
  _ -> a

-- | The enemy's attack, with what of its keywords are its attacks'.
attackWith :: [Keyword] -> EnemyId -> CharacterId -> Natural -> Attack
attackWith keywords enemyId characterId = flip (foldr gainKeyword) keywords . attack enemyId characterId

-- | The attack's damage to the character it is against.
attackDamageTo :: Attack -> Natural -> Damage
attackDamageTo a n = (toDamage n FromAttack) {damagePiercing = attackPiercing a}
