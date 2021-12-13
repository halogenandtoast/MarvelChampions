{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion.Attrs
  ( module Marvel.Minion.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Code as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Message as X
import Marvel.Modifier as X
import Marvel.Query as X
import Marvel.Question as X
import Marvel.Queue as X
import Marvel.Source as X
import Marvel.Stats as X
import Marvel.Target as X

import Data.HashSet qualified as HashSet
import Marvel.Attack
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Window qualified as W

class IsMinion a

type MinionCard a = CardBuilder (IdentityId, MinionId) a

minionRemainingHitPoints :: MinionAttrs -> Natural
minionRemainingHitPoints attrs =
  subtractNatural (minionDamage attrs) (unHp $ minionHitPoints attrs)

data MinionAttrs = MinionAttrs
  { minionId :: MinionId
  , minionCardDef :: CardDef
  , minionDamage :: Natural
  , minionHitPoints :: HP Natural
  , minionScheme :: Sch
  , minionAttack :: Atk
  , minionEngagedIdentity :: IdentityId
  , minionStunned :: Bool
  , minionConfused :: Bool
  , minionTough :: Bool
  , minionAttacking :: Maybe Attack
  , minionUpgrades :: HashSet UpgradeId
  , minionAttachments :: HashSet AttachmentId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''MinionAttrs

instance HasCardCode MinionAttrs where
  toCardCode = toCardCode . minionCardDef

minionWith
  :: (MinionAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP Natural
  -> (MinionAttrs -> MinionAttrs)
  -> CardBuilder (IdentityId, MinionId) a
minionWith f cardDef sch atk hp g = minion (f . g) cardDef sch atk hp

minion
  :: (MinionAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP Natural
  -> CardBuilder (IdentityId, MinionId) a
minion f cardDef sch atk hp = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, mid) -> f $ MinionAttrs
    { minionId = mid
    , minionCardDef = cardDef
    , minionDamage = 0
    , minionAttack = atk
    , minionScheme = sch
    , minionHitPoints = hp
    , minionEngagedIdentity = ident
    , minionStunned = False
    , minionConfused = False
    , minionTough = False
    , minionAttacking = Nothing
    , minionUpgrades = mempty
    , minionAttachments = mempty
    }
  }

instance Entity MinionAttrs where
  type EntityId MinionAttrs = MinionId
  type EntityAttrs MinionAttrs = MinionAttrs
  toId = minionId
  toAttrs = id

instance IsSource MinionAttrs where
  toSource = MinionSource . toId

instance IsTarget MinionAttrs where
  toTarget = MinionTarget . toId

instance IsCard MinionAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unMinionId $ toId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef MinionAttrs where
  getCardDef = minionCardDef

getModifiedHitPoints :: MonadGame env m => MinionAttrs -> m Natural
getModifiedHitPoints attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unHp $ minionHitPoints attrs) modifiers
 where
  applyModifier (HitPointModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedAttack :: MonadGame env m => MinionAttrs -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ minionAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

instance RunMessage MinionAttrs where
  runMessage msg attrs = case msg of
    MinionMessage minionId msg' | minionId == toId attrs ->
      runMinionMessage msg' attrs
    _ -> pure attrs

toEnemyId :: MinionAttrs -> EnemyId
toEnemyId = EnemyMinionId . toId

runMinionMessage
  :: MonadGame env m => MinionMessage -> MinionAttrs -> m MinionAttrs
runMinionMessage msg attrs = case msg of
  MinionHealed n -> do
    pure $ attrs & damageL %~ subtractNatural n
  MinionHealAllDamage -> do
    pure $ attrs & damageL .~ 0
  MinionDamaged source damage -> if minionTough attrs
    then pure $ attrs & toughL .~ False
    else do
      hitPoints <- getModifiedHitPoints attrs
      when (damageAmount damage + minionDamage attrs >= hitPoints) $ do
        let overkill = damageAmount damage - (hitPoints - minionDamage attrs)
        when (overkill > 0 && damageOverkill damage) $ do
          villain <- selectJust ActiveVillain
          push $ VillainMessage villain (VillainDamaged source (toDamage overkill FromOverkill))
        pushAll
          [ CheckWindows [W.Window W.When $ W.DefeatedMinion (toId attrs)]
          , MinionMessage (toId attrs) MinionDefeated
          , CheckWindows [W.Window W.After $ W.DefeatedMinion (toId attrs)]
          ]
      pure $ attrs & damageL +~ (damageAmount damage)
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
  MinionBecomeTough-> pure $ attrs & toughL .~ True
  MinionDefendedBy characterId ->
    pure $ attrs & attackingL . _Just . attackCharacterL .~ characterId
  AttachedToMinion attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToMinion upgradeId ->
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  RevealMinion _ -> pure attrs
  MinionEngagedIdentity ident -> do
    isHero <- identityMatches HeroIdentity ident
    when
      (isHero && Quickstrike `member` cdKeywords (getCardDef attrs))
      (push $ MinionMessage (toId attrs) $ MinionAttacks ident)
    pure attrs
  MinionDefeated -> do
    pushAll
      $ map (RemoveFromPlay . UpgradeTarget) (toList $ minionUpgrades attrs)
      <> [ RemoveFromPlay (toTarget attrs)
         , IdentityMessage
           (minionEngagedIdentity attrs)
           (MinionDisengaged $ toId attrs)
         ]
    pure attrs
  MinionSchemes -> if minionConfused attrs
    then pure $ attrs & confusedL .~ False
    else do
      push $ MinionMessage (toId attrs) MinionSchemed
      pure attrs
  MinionAttacks ident -> if minionStunned attrs
    then pure $ attrs & stunnedL .~ False
    else do
      pushAll
        [ CheckWindows
          [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
        , MinionMessage (toId attrs) (MinionBeginAttack ident)
        ]
      pure attrs
  MinionBeginAttack ident -> do
    atk <- getModifiedAttack attrs
    pushAll
      [ CheckWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
      , DeclareDefense ident (toEnemyId attrs)
      , MinionMessage (toId attrs) MinionAttacked
      ]
    pure
      $ attrs
      & attackingL
      ?~ attack (toEnemyId attrs) (IdentityCharacter ident) atk
  MinionSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        let threat = unSch (minionScheme attrs)
        pushAll
          [ CheckWindows
            [ W.Window W.Would $ W.ThreatPlaced
                W.ThreatFromMinion
                (SchemeMainSchemeId mainSchemeId)
                threat
            ]
          , MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat
          ]
        pure attrs
      _ -> error "Not the main scheme"
  MinionAttacked -> do
    case minionAttacking attrs of
      Nothing -> error "No current attack"
      Just attack' -> case attackCharacter attack' of
        IdentityCharacter ident ->
          push $ IdentityMessage ident $ IdentityWasAttacked attack'
        AllyCharacter ident ->
          push $ AllyMessage ident $ AllyWasAttacked attack'
        _ -> error "Invalid damage target"
    pure attrs
