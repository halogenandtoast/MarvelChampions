{-# LANGUAGE TemplateHaskell #-}
module Marvel.Villain.Attrs
  ( module Marvel.Villain.Attrs
  , module X
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Boost
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Entity as X
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Id as X (VillainId)
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villainWith
  :: (VillainAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP GameValue
  -> (VillainAttrs -> VillainAttrs)
  -> VillainCard a
villainWith f cardDef sch atk startingHp g =
  villain (f . g) cardDef sch atk startingHp

villain
  :: (VillainAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP GameValue
  -> VillainCard a
villain f cardDef sch atk startingHp = CardBuilder
  { cbCardCode = toCardCode cardDef
  , cbCardBuilder = \ident -> f $ VillainAttrs
    { villainId = ident
    , villainCardDef = cardDef
    , villainStartingHp = startingHp
    , villainMaxHp = HP 1
    , villainHp = HP 1
    , villainScheme = sch
    , villainAttack = atk
    , villainStunned = False
    , villainConfused = False
    , villainBoostCards = mempty
    , villainBoost = 0
    , villainAttacking = Nothing
    , villainAttachments = mempty
    , villainUpgrades = mempty
    , villainStage = 1
    }
  }

data VillainAttrs = VillainAttrs
  { villainId :: VillainId
  , villainCardDef :: CardDef
  , villainHp :: HP Int
  , villainStartingHp :: HP GameValue
  , villainMaxHp :: HP Int
  , villainScheme :: Sch
  , villainAttack :: Atk
  , villainStunned :: Bool
  , villainConfused :: Bool
  , villainBoostCards :: [EncounterCard]
  , villainBoost :: Natural
  , villainAttacking :: Maybe CharacterId
  , villainAttachments :: HashSet AttachmentId
  , villainUpgrades :: HashSet UpgradeId
  , villainStage :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''VillainAttrs

instance Entity VillainAttrs where
  type EntityId VillainAttrs = VillainId
  type EntityAttrs VillainAttrs = VillainAttrs
  toId = villainId
  toAttrs = id

runVillainMessage
  :: MonadGame env m => VillainMessage -> VillainAttrs -> m VillainAttrs
runVillainMessage msg attrs = case msg of
  VillainAdvanced -> do
    push (VillainMessage (toId attrs) SetVillainHp)
    pure attrs
  SetVillainHp -> do
    hp <- fromGameValue (unHp $ villainStartingHp attrs)
    pure $ attrs & hpL .~ HP hp & maxHpL .~ HP hp
  VillainHealed n -> do
    pure
      $ attrs
      & hpL
      %~ HP
      . min (unHp $ villainMaxHp attrs)
      . (+ fromIntegral n)
      . unHp
  VillainDamaged _ n -> do
    when
      (subtractNatural n (fromIntegral . unHp $ villainHp attrs) == 0)
      (push $ VillainMessage (toId attrs) VillainDefeated)
    pure $ attrs & hpL %~ HP . max 0 . subtract (fromIntegral n) . unHp
  VillainDefeated -> do
    push (GameOver Won)
    pure attrs
  VillainStunned _ -> pure $ attrs & stunnedL .~ True
  VillainConfused _ -> pure $ attrs & confusedL .~ True
  VillainSchemes -> if villainConfused attrs
    then pure $ attrs & confusedL .~ False
    else do
      pushAll
        [ DealBoost (toTarget attrs)
        , VillainMessage (toId attrs) VillainFlipBoostCards
        , VillainMessage (toId attrs) VillainSchemed
        ]
      pure attrs
  VillainAttacks ident -> if villainStunned attrs
    then pure $ attrs & stunnedL .~ False
    else do
      pushAll
        [ CheckWindows
          [Window When $ EnemyAttack (EnemyVillainId $ toId attrs) ident]
        , VillainMessage (toId attrs) (VillainBeginAttack ident)
        ]
      pure attrs
  VillainBeginAttack ident -> do
    pushAll
      [ DealBoost (toTarget attrs)
      , DeclareDefense ident (EnemyVillainId (toId attrs))
      , VillainMessage (toId attrs) VillainFlipBoostCards
      , VillainMessage (toId attrs) VillainAttacked
      ]
    pure $ attrs & attackingL ?~ IdentityCharacter ident
  VillainDefendedBy characterId -> pure $ attrs & attackingL ?~ characterId
  VillainSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        let threat = unSch (villainScheme attrs) + villainBoost attrs
        push (MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat)
        pure $ attrs & boostL .~ 0
  VillainAttacked -> do
    let dmg = unAtk (villainAttack attrs) + villainBoost attrs
    case villainAttacking attrs of
      Just (IdentityCharacter ident) -> pushAll
        [ CheckWindows [Window When $ IdentityTakeDamage ident FromAttack dmg]
        , IdentityMessage ident $ IdentityDamaged (toSource attrs) dmg
        ]
      Just (AllyCharacter ident) ->
        push (AllyMessage ident $ AllyDamaged (toSource attrs) dmg)
      _ -> error "Invalid damage target"
    pure $ attrs & boostL .~ 0
  DealtBoost c -> pure $ attrs & boostCardsL %~ (c :)
  AttachedToVillain attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  UpgradeAttachedToVillain upgradeId -> do
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  VillainFlipBoostCards -> do
    let
      boost = foldr
        ((+) . boostCount . cdBoostIcons . ecCardDef)
        0
        (villainBoostCards attrs)
    pushAll $ map DiscardedEncounterCard (villainBoostCards attrs)
    pure $ attrs & boostCardsL .~ mempty & boostL .~ boost

instance RunMessage VillainAttrs where
  runMessage msg attrs = case msg of
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    VillainMessage villainId msg' | villainId == toId attrs ->
      runVillainMessage msg' attrs
    _ -> pure attrs

instance IsTarget VillainAttrs where
  toTarget = VillainTarget . villainId

instance IsSource VillainAttrs where
  toSource = VillainSource . villainId

advanceVillainTo
  :: (Entity a, EntityAttrs a ~ VillainAttrs, Coercible a VillainAttrs)
  => VillainCard a
  -> VillainAttrs
  -> a
advanceVillainTo newVillain VillainAttrs {..} = cbCardBuilder
  (update <$> newVillain)
  villainId
 where
  update =
    coerce
      . ((confusedL .~ villainConfused)
        . (stunnedL .~ villainStunned)
        . (attachmentsL .~ villainAttachments)
        . toAttrs
        )

