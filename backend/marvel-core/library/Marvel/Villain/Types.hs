module Marvel.Villain.Types
  ( module Marvel.Villain.Types
  , module X
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability.Type
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
import Marvel.Message qualified as Msg
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W
import Text.Show qualified

data Villain = forall a . IsVillain a => Villain a

instance Show Villain where
  show (Villain a) = show a

instance ToJSON Villain where
  toJSON (Villain a) = toJSON a

instance Eq Villain where
  Villain (a :: a) == Villain (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeVillainCard = forall a . IsVillain a => SomeVillainCard
  (VillainCard a)

liftVillainCard :: (forall a . VillainCard a -> b) -> SomeVillainCard -> b
liftVillainCard f (SomeVillainCard a) = f a

someVillainCardCode :: SomeVillainCard -> CardCode
someVillainCardCode = liftVillainCard cbCardCode

villainDamage :: Villain -> Natural
villainDamage v = fromIntegral . max 0 $ unHp (villainMaxHp attrs) - unHp
  (villainHp attrs)
  where attrs = toAttrs v

villainIsTough :: Villain -> Bool
villainIsTough = villainTough . toAttrs

villainAttackDetails :: Villain -> Maybe Attack
villainAttackDetails = villainAttacking . toAttrs

instance HasAbilities Villain where
  getAbilities (Villain a) = getAbilities a

instance RunMessage Villain where
  runMessage msg (Villain a) = Villain <$> runMessage msg a

instance HasCardDef Villain where
  getCardDef = getCardDef . toAttrs

instance Entity Villain where
  type Id Villain = VillainId
  data Attrs Villain = VillainAttrs
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
  data Field Villain :: Type -> Type where
    VillainId :: Field Villain VillainId
    VillainCardDef :: Field Villain CardDef
    VillainHp :: Field Villain (HP Int)
    VillainStartingHp :: Field Villain (HP GameValue)
    VillainMaxHp :: Field Villain (HP Int)
    VillainScheme :: Field Villain Sch
    VillainAttack :: Field Villain Atk
    VillainStunned :: Field Villain Bool
    VillainConfused :: Field Villain Bool
    VillainTough :: Field Villain Bool
    VillainBoostCards :: Field Villain [EncounterCard]
    VillainBoost :: Field Villain Natural
    VillainAttacking :: Field Villain (Maybe Attack)
    VillainAttachments :: Field Villain (HashSet AttachmentId)
    VillainUpgrades :: Field Villain (HashSet UpgradeId)
    VillainStage :: Field Villain Natural
  toId = villainId . toAttrs
  toAttrs (Villain a) = toVillainAttrs a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a, HasAbilities a) => IsVillain a where
  toVillainAttrs :: a -> Attrs Villain
  default toVillainAttrs :: Coercible a (Attrs Villain) => a -> Attrs Villain
  toVillainAttrs = coerce
  overVillainAttrs :: (Attrs Villain -> Attrs Villain) -> a -> a
  default overVillainAttrs :: Coercible a (Attrs Villain) => (Attrs Villain -> Attrs Villain) -> a -> a
  overVillainAttrs f = coerce f

type VillainCard a = CardBuilder VillainId a

villainWith
  :: (Attrs Villain -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP GameValue
  -> (Attrs Villain -> Attrs Villain)
  -> VillainCard a
villainWith f cardDef sch atk startingHp g =
  villain (f . g) cardDef sch atk startingHp

villain
  :: (Attrs Villain -> a)
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
    , villainTough = False
    , villainBoostCards = mempty
    , villainBoost = 0
    , villainAttacking = Nothing
    , villainAttachments = mempty
    , villainUpgrades = mempty
    , villainStage = 1
    }
  }

-- makeLensesWith suffixedFields ''VillainAttrs

stageL :: Lens' (Attrs Villain) Natural
stageL = lens villainStage $ \m x -> m { villainStage = x }

hpL :: Lens' (Attrs Villain) (HP Int)
hpL = lens villainHp $ \m x -> m { villainHp = x }

maxHpL :: Lens' (Attrs Villain) (HP Int)
maxHpL = lens villainMaxHp $ \m x -> m { villainMaxHp = x }

upgradesL :: Lens' (Attrs Villain) (HashSet UpgradeId)
upgradesL = lens villainUpgrades $ \m x -> m { villainUpgrades = x }

attachmentsL :: Lens' (Attrs Villain) (HashSet AttachmentId)
attachmentsL = lens villainAttachments $ \m x -> m { villainAttachments = x }

toughL :: Lens' (Attrs Villain) Bool
toughL = lens villainTough $ \m x -> m { villainTough = x }

stunnedL :: Lens' (Attrs Villain) Bool
stunnedL = lens villainStunned $ \m x -> m { villainStunned = x }

confusedL :: Lens' (Attrs Villain) Bool
confusedL = lens villainConfused $ \m x -> m { villainConfused = x }

attackingL :: Lens' (Attrs Villain) (Maybe Attack)
attackingL = lens villainAttacking $ \m x -> m { villainAttacking = x }

boostL :: Lens' (Attrs Villain) Natural
boostL = lens villainBoost $ \m x -> m { villainBoost = x }

boostCardsL :: Lens' (Attrs Villain) [EncounterCard]
boostCardsL = lens villainBoostCards $ \m x -> m { villainBoostCards = x }

instance HasCardDef (Attrs Villain) where
  getCardDef = villainCardDef

toEnemyId :: Attrs Villain -> EnemyId
toEnemyId = EnemyVillainId . villainId

getModifiedKeywords :: MonadGame env m => Attrs Villain -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

runVillainMessage
  :: MonadGame env m => VillainMessage -> Attrs Villain -> m (Attrs Villain)
runVillainMessage msg attrs = case msg of
  VillainAdvanced -> do
    pushAll
      [ VillainMessage (villainId attrs) SetVillainHp
      , CheckWindows
        [W.Window W.When $ W.RevealVillain (villainId attrs) W.RevealedFromVillain]
      ]
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
  VillainDamaged _ damage -> if villainTough attrs
    then pure $ attrs & toughL .~ False
    else do
      keywords <- getModifiedKeywords attrs
      when
        (subtractNatural
            (damageAmount damage)
            (fromIntegral . unHp $ villainHp attrs)
        == 0
        )
        (push $ VillainMessage (villainId attrs) VillainDefeated)

      for_ keywords $ \case
        Retaliate n -> case damageSource damage of
          FromPlayerAttack ident -> push $ IdentityMessage
            ident
            (IdentityDamaged (toSource attrs) (toDamage n FromRetaliate))
          FromAllyAttack ident -> push $ AllyMessage
            ident
            (AllyDamaged (toSource attrs) (toDamage n FromRetaliate))
          _ -> pure ()
        _ -> pure ()
      pure
        $ attrs
        & hpL
        %~ HP
        . max 0
        . subtract (fromIntegral $ damageAmount damage)
        . unHp
  VillainDefeated -> do
    push (GameOver Won)
    pure attrs
  VillainBecomeTough -> pure $ attrs & toughL .~ True
  Msg.VillainStunned _ -> pure $ attrs & stunnedL .~ True
  Msg.VillainConfused _ -> pure $ attrs & confusedL .~ True
  VillainSchemes -> if villainConfused attrs
    then pure $ attrs & confusedL .~ False
    else do
      pushAll
        [ DealBoost (toTarget attrs)
        , VillainMessage (villainId attrs) VillainFlipBoostCards
        , VillainMessage (villainId attrs) VillainSchemed
        , ClearBoosts
        ]
      pure attrs
  VillainAttacks ident -> if villainStunned attrs
    then pure $ attrs & stunnedL .~ False
    else do
      pushAll
        [ CheckWindows
          [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
        , VillainMessage (villainId attrs) (VillainBeginAttack ident)
        , VillainMessage (villainId attrs) VillainEndAttack
        , ClearBoosts
        ]
      pure attrs
  VillainBeginAttack ident -> do
    atk <- getModifiedAttack attrs
    pushAll
      [ CheckWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
      , DealBoost (toTarget attrs)
      , DeclareDefense ident (toEnemyId attrs) AnyDefense
      , VillainMessage (villainId attrs) VillainFlipBoostCards
      , VillainMessage (villainId attrs) VillainAttacked
      , CheckWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
      ]
    pure
      $ attrs
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
            [ W.Window W.Would $ W.ThreatPlaced
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
    let
      boost = foldr
        ((+) . boostCount . cdBoostIcons . ecCardDef)
        0
        (villainBoostCards attrs)
    pushAll
      $ map
          (`RevealBoostCard` EnemyVillainId (villainId attrs))
          (villainBoostCards attrs)
      <> [VillainMessage (villainId attrs) VillainCheckAdditionalBoosts]
    pure
      $ attrs
      & (boostCardsL .~ mempty)
      & (boostL .~ boost)
      & (attackingL . _Just . attackDamageL +~ boost)
  VillainCheckAdditionalBoosts -> do
    unless
      (null $ villainBoostCards attrs)
      (push $ VillainMessage (villainId attrs) VillainFlipBoostCards)
    pure attrs

instance RunMessage (Attrs Villain) where
  runMessage msg attrs = case msg of
    AttachmentRemoved attachmentId -> do
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    VillainMessage ident msg' | ident == villainId attrs ->
      runVillainMessage msg' attrs
    _ -> pure attrs

instance IsTarget (Attrs Villain) where
  toTarget = VillainTarget . villainId

instance IsSource (Attrs Villain) where
  toSource = VillainSource . villainId

advanceVillainTo :: IsVillain a => VillainCard a -> Attrs Villain -> a
advanceVillainTo newVillain VillainAttrs {..} = overVillainAttrs update $ cbCardBuilder newVillain villainId
 where
  update =
      (confusedL .~ villainConfused)
        . (stunnedL .~ villainStunned)
        . (toughL %~ (|| villainTough))
        . (attachmentsL .~ villainAttachments)
        . (upgradesL .~ villainUpgrades)

getModifiedAttack :: MonadGame env m => Attrs Villain -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ villainAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedScheme :: MonadGame env m => Attrs Villain -> m Natural
getModifiedScheme attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unSch $ villainScheme attrs) modifiers
 where
  applyModifier (SchemeModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id
