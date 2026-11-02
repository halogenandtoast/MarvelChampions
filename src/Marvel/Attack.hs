module Marvel.Attack where

import Marvel.Prelude

import Data.Aeson.KeyMap qualified as KeyMap

import Marvel.Card.Id (CardId)
import Marvel.Damage
import Marvel.Id
import Marvel.Keyword
import Marvel.Ref

data Attack = Attack
  { attackCharacter :: CharacterId
  , attackBlanksDefender :: Bool
  , attackCompletionId :: Maybe CardId
  , attackIsSimultaneousAdditional :: Bool
  , attackSimultaneousGroupId :: Maybe CardId
  , attackOverkill :: Bool
  , attackDamage :: Natural
  , attackBaseReduction :: Natural
  -- ^ the negative part of the signed base plus boosts, retained until the final
  -- attack value is calculated together with attack-specific changes
  , attackEnemy :: EnemyId
  , attackDefended :: Bool
  , attackPiercing :: Bool
  , attackRanged :: Bool
  , attackIdentityDamageCaps :: [(IdentityId, Natural, Natural)]
  -- ^ identity, maximum damage from this attack, and damage actually taken so far
  , attackPrevention :: Natural
  , attackUsesAttackForDefense :: Maybe IdentityId
  , attackThreatRemoval :: Maybe IdentityId
  , attackPrevented :: Bool
  -- ^ "prevent all damage from this attack"
  , attackDividedEvenly :: Bool
  , attackIndirect :: Bool
  -- ^ the attack deals indirect damage (Starshark): its damage, past a hero's defense,
  -- is assigned among the characters the attacked player controls (RR Indirect Damage)
  , attackChange :: Int
  -- ^ "gets +X/-X ATK for that attack", applied after its boost icons
  , attackDamageRedirect :: Maybe EnemyId
  , attackTarget :: Maybe CharacterId
  -- ^ the character it was against before a defender was declared
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

-- | An attack saved before attacks could deal indirect damage deals direct damage.
instance FromJSON Attack where
  parseJSON = withObject "Attack" \o ->
    genericParseJSON defaultOptions (Object $ KeyMap.union o $ KeyMap.fromList [("attackIdentityDamageCaps", toJSON ([] :: [(IdentityId, Natural, Natural)])), ("attackSimultaneousGroupId", toJSON (Nothing :: Maybe CardId)), ("attackIsSimultaneousAdditional", toJSON False), ("attackBlanksDefender", toJSON False), ("attackCompletionId", toJSON (Nothing :: Maybe CardId)), ("attackBaseReduction", toJSON (0 :: Natural)), ("attackDamageRedirect", toJSON (Nothing :: Maybe EnemyId)), ("attackUsesAttackForDefense", toJSON (Nothing :: Maybe IdentityId)), ("attackPrevention", toJSON (0 :: Natural)), ("attackThreatRemoval", toJSON (Nothing :: Maybe IdentityId)), ("attackIndirect", toJSON False), ("attackDividedEvenly", toJSON False)])

attackSource :: Attack -> Source
attackSource a = case attackEnemy a of
  EnemyMinionId mid -> MinionRef mid
  EnemyAttachmentId aid -> AttachmentRef aid
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

-- | Defended by the character: the attack is against it now, and was against its target.
defendedBy :: CharacterId -> Attack -> Attack
defendedBy c a = a {attackCharacter = c, attackDefended = True, attackTarget = attackTarget a <|> Just (attackCharacter a)}

attackDamageL :: Lens' Attack Natural
attackDamageL = lens attackDamage $ \m x -> m {attackDamage = x}

attack :: EnemyId -> CharacterId -> Natural -> Attack
attack enemyId characterId dmg =
  Attack
    { attackCharacter = characterId
    , attackBlanksDefender = False
    , attackCompletionId = Nothing
    , attackIsSimultaneousAdditional = False
    , attackSimultaneousGroupId = Nothing
    , attackOverkill = False
    , attackDamage = dmg
    , attackBaseReduction = 0
    , attackEnemy = enemyId
    , attackDefended = False
    , attackPiercing = False
    , attackRanged = False
    , attackIdentityDamageCaps = []
    , attackPrevention = 0
    , attackUsesAttackForDefense = Nothing
    , attackThreatRemoval = Nothing
    , attackPrevented = False
    , attackDividedEvenly = False
    , attackIndirect = False
    , attackChange = 0
    , attackDamageRedirect = Nothing
    , attackTarget = Nothing
    }

-- | The attack's ATK: its damage, boost icons included, and what it gets for this attack.
attackValue :: Attack -> Natural
attackValue a = fromInteger $ max 0
  (toInteger (attackDamage a) - toInteger (attackBaseReduction a) + toInteger (attackChange a))

-- | Refresh only the signed base and boosts. An attack's own changes, defense,
-- prevention, target, and gained keywords remain its own.
withAttackBase :: Integer -> Attack -> Attack
withAttackBase value a = a
  { attackDamage = fromInteger (max 0 value)
  , attackBaseReduction = fromInteger (max 0 $ negate value)
  }

-- | The damage the attack deals: none once all of it is prevented.
attackDamageDealt :: Attack -> Natural
attackDamageDealt a = if attackPrevented a then 0 else subtractNatural (attackPrevention a) (attackValue a)

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
    { attackBlanksDefender = attackBlanksDefender a || attackBlanksDefender old
    , attackCompletionId = attackCompletionId old <|> attackCompletionId a
    , attackOverkill = attackOverkill a || attackOverkill old
    , attackPiercing = attackPiercing a || attackPiercing old
    , attackRanged = attackRanged a || attackRanged old
    , attackPrevented = attackPrevented a || attackPrevented old
    , attackIdentityDamageCaps = attackIdentityDamageCaps old <> attackIdentityDamageCaps a
    , attackPrevention = attackPrevention a + attackPrevention old
    , attackUsesAttackForDefense = attackUsesAttackForDefense old <|> attackUsesAttackForDefense a
    , attackThreatRemoval = attackThreatRemoval old <|> attackThreatRemoval a
    , attackDefended = attackDefended a || attackDefended old
    , attackCharacter = if attackDefended old then attackCharacter old else attackCharacter a
    , attackDamageRedirect = attackDamageRedirect old <|> attackDamageRedirect a
    , attackTarget = attackTarget old <|> attackTarget a
    , attackChange = attackChange a + attackChange old
    }
