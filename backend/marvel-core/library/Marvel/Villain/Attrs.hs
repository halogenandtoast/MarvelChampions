{-# LANGUAGE TemplateHaskell #-}

module Marvel.Villain.Attrs (
  module Marvel.Villain.Attrs,
  module X,
) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Attack
import Marvel.Boost
import Marvel.Card
import Marvel.Damage
import Marvel.Entity as X
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Id as X (VillainId)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villainWith ::
  (VillainAttrs -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP GameValue ->
  (VillainAttrs -> VillainAttrs) ->
  VillainCard a
villainWith f cardDef sch atk startingHp g =
  villain (f . g) cardDef sch atk startingHp

villain ::
  (VillainAttrs -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP GameValue ->
  VillainCard a
villain f cardDef sch atk startingHp =
  CardBuilder
    { cbCardCode = toCardCode cardDef
    , cbCardBuilder = \ident ->
        f $
          VillainAttrs
            { villainId = ident
            , villainCardDef = cardDef
            , villainStartingHp = startingHp
            , villainMaxHp = HP 1
            , villainHp = HP 1
            , villainScheme = sch
            , villainAttack = atk
            , villainStunned = False
            , villainConfused = False
            , villainTough = False
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
  , villainTough :: Bool
  , villainBoostCards :: [EncounterCard]
  , villainBoost :: Natural
  , villainAttacking :: Maybe Attack
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

instance HasCardDef VillainAttrs where
  getCardDef = villainCardDef

toEnemyId :: VillainAttrs -> EnemyId
toEnemyId = EnemyVillainId . toId

getModifiedKeywords :: MonadGame env m => VillainAttrs -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

runVillainMessage ::
  MonadGame env m => VillainMessage -> VillainAttrs -> m VillainAttrs
runVillainMessage msg attrs = case msg of
  VillainAdvanced -> do
    pushAll
      [ VillainMessage (toId attrs) SetVillainHp
      , CheckWindows
          [W.Window W.When $ W.RevealVillain (toId attrs) W.RevealedFromVillain]
      ]
    pure attrs
  SetVillainHp -> do
    hp <- fromGameValue (unHp $ villainStartingHp attrs)
    pure $ attrs & hpL .~ HP hp & maxHpL .~ HP hp
  VillainHealed n -> do
    pure $
      attrs
        & hpL
        %~ HP
          . min (unHp $ villainMaxHp attrs)
          . (+ fromIntegral n)
          . unHp
  VillainDamaged _ damage ->
    if villainTough attrs
      then pure $ attrs & toughL .~ False
      else do
        keywords <- getModifiedKeywords attrs
        when
          ( subtractNatural
              (damageAmount damage)
              (fromIntegral . unHp $ villainHp attrs)
              == 0
          )
          (push $ VillainMessage (toId attrs) VillainDefeated)

        for_ keywords $ \case
          Retaliate n -> case damageSource damage of
            FromPlayerAttack ident ->
              push $
                IdentityMessage
                  ident
                  (IdentityDamaged (toSource attrs) (toDamage n FromRetaliate))
            FromAllyAttack ident ->
              push $
                AllyMessage
                  ident
                  (AllyDamaged (toSource attrs) (toDamage n FromRetaliate))
            _ -> pure ()
          _ -> pure ()
        pure $
          attrs
            & hpL
            %~ HP
              . max 0
              . subtract (fromIntegral $ damageAmount damage)
              . unHp
  VillainDefeated -> do
    push (GameOver Won)
    pure attrs
  VillainBecomeTough -> pure $ attrs & toughL .~ True
  VillainStunned _ -> pure $ attrs & stunnedL .~ True
  VillainConfused _ -> pure $ attrs & confusedL .~ True
  VillainSchemes ->
    if villainConfused attrs
      then pure $ attrs & confusedL .~ False
      else do
        pushAll
          [ DealBoost (toTarget attrs)
          , VillainMessage (toId attrs) VillainFlipBoostCards
          , VillainMessage (toId attrs) VillainSchemed
          , ClearBoosts
          ]
        pure attrs
  VillainAttacks ident ->
    if villainStunned attrs
      then pure $ attrs & stunnedL .~ False
      else do
        pushAll
          [ CheckWindows
              [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
          , VillainMessage (toId attrs) (VillainBeginAttack ident)
          , VillainMessage (toId attrs) VillainEndAttack
          , ClearBoosts
          ]
        pure attrs
  VillainBeginAttack ident -> do
    atk <- getModifiedAttack attrs
    pushAll
      [ CheckWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
      , DealBoost (toTarget attrs)
      , DeclareDefense ident (toEnemyId attrs) AnyDefense
      , VillainMessage (toId attrs) VillainFlipBoostCards
      , VillainMessage (toId attrs) VillainAttacked
      , CheckWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
      ]
    pure $
      attrs
        & attackingL
        ?~ attack (toEnemyId attrs) (IdentityCharacter ident) atk
  VillainEndAttack -> pure $ attrs & attackingL .~ Nothing
  VillainAttackGainOverkill ->
    pure $ attrs & attackingL . _Just . attackOverkillL .~ True
  VillainDefendedBy characterId ->
    pure $ attrs & attackingL . _Just . attackCharacterL .~ characterId
  VillainSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        sch <- getModifiedScheme attrs
        let threat = sch + villainBoost attrs
        pushAll
          [ CheckWindows
              [ W.Window W.Would $
                  W.ThreatPlaced
                    W.ThreatFromVillain
                    (SchemeMainSchemeId mainSchemeId)
                    threat
              ]
          , MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat
          ]
        pure $ attrs & boostL .~ 0
      _ -> error "not Main scheme"
  VillainAttacked -> do
    case villainAttacking attrs of
      Nothing -> error "No current attack"
      Just attack' -> case attackCharacter attack' of
        IdentityCharacter ident ->
          push $ IdentityMessage ident $ IdentityWasAttacked attack'
        AllyCharacter ident ->
          push $ AllyMessage ident $ AllyWasAttacked attack'
        _ -> error "Invalid damage target"
    pure $ attrs & boostL .~ 0
  VillainDealtBoost c -> pure $ attrs & boostCardsL %~ (c :)
  AttachedToVillain attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToVillain upgradeId -> do
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  VillainFlipBoostCards -> do
    let boost =
          foldr
            ((+) . boostCount . cdBoostIcons . ecCardDef)
            0
            (villainBoostCards attrs)
    pushAll $
      map (`RevealBoostCard` EnemyVillainId (toId attrs)) (villainBoostCards attrs)
        <> [VillainMessage (toId attrs) VillainCheckAdditionalBoosts]
    pure $
      attrs
        & (boostCardsL .~ mempty)
        & (boostL .~ boost)
        & (attackingL . _Just . attackDamageL +~ boost)
  VillainCheckAdditionalBoosts -> do
    unless
      (null $ villainBoostCards attrs)
      (push $ VillainMessage (toId attrs) VillainFlipBoostCards)
    pure attrs

instance RunMessage VillainAttrs where
  runMessage msg attrs = case msg of
    AttachmentRemoved attachmentId -> do
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    VillainMessage villainId msg'
      | villainId == toId attrs ->
        runVillainMessage msg' attrs
    _ -> pure attrs

instance IsTarget VillainAttrs where
  toTarget = VillainTarget . villainId

instance IsSource VillainAttrs where
  toSource = VillainSource . villainId

advanceVillainTo ::
  (Entity a, EntityAttrs a ~ VillainAttrs, Coercible a VillainAttrs) =>
  VillainCard a ->
  VillainAttrs ->
  a
advanceVillainTo newVillain VillainAttrs {..} =
  cbCardBuilder
    (update <$> newVillain)
    villainId
 where
  update =
    coerce
      . ( (confusedL .~ villainConfused)
            . (stunnedL .~ villainStunned)
            . (toughL %~ (|| villainTough))
            . (attachmentsL .~ villainAttachments)
            . (upgradesL .~ villainUpgrades)
            . toAttrs
        )

getModifiedAttack :: MonadGame env m => VillainAttrs -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ villainAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedScheme :: MonadGame env m => VillainAttrs -> m Natural
getModifiedScheme attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unSch $ villainScheme attrs) modifiers
 where
  applyModifier (SchemeModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id
