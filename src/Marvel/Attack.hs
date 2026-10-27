module Marvel.Attack where

import Marvel.Prelude

import Data.Aeson.KeyMap qualified as KeyMap

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
  , attackIndirect :: Bool
  -- ^ the attack deals indirect damage (Starshark): its damage, past a hero's defense,
  -- is assigned among the characters the attacked player controls (RR Indirect Damage)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

-- | An attack saved before attacks could deal indirect damage deals direct damage.
instance FromJSON Attack where
  parseJSON = withObject "Attack" \o ->
    genericParseJSON defaultOptions (Object $ KeyMap.insertWith (\_ existing -> existing) "attackIndirect" (toJSON False) o)

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
    , attackIndirect = False
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

-- | The attack begun afresh keeps the keywords it gained as it was declared, in the
-- windows before it began.
keepingGains :: Maybe Attack -> Attack -> Attack
keepingGains Nothing a = a
keepingGains (Just old) a =
  a
    { attackOverkill = attackOverkill a || attackOverkill old
    , attackPiercing = attackPiercing a || attackPiercing old
    , attackRanged = attackRanged a || attackRanged old
    }
