module Marvel.Minion.Types
  ( module Marvel.Minion.Types
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Code as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Modifier as X
import Marvel.Query as X
import Marvel.Queue as X
import Marvel.Source as X
import Marvel.Stats as X
import Marvel.Target as X

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability.Type
import Marvel.Attack
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Trait
import Marvel.Window qualified as W
import Text.Show qualified

data Minion = forall a . IsMinion a => Minion a

instance Show Minion where
  show (Minion a) = show a

instance ToJSON Minion where
  toJSON (Minion a) = toJSON a

instance Eq Minion where
  Minion (a :: a) == Minion (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeMinionCard = forall a . IsMinion a => SomeMinionCard (MinionCard a)

liftMinionCard :: (forall a . MinionCard a -> b) -> SomeMinionCard -> b
liftMinionCard f (SomeMinionCard a) = f a

someMinionCardCode :: SomeMinionCard -> CardCode
someMinionCardCode = liftMinionCard cbCardCode

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ MinionAttrs, EntityId a ~ MinionId, HasModifiersFor a, RunMessage a, HasAbilities a) => IsMinion a

type MinionCard a = CardBuilder (IdentityId, MinionId) a

getMinionDamage :: Minion -> Natural
getMinionDamage = minionDamage . toAttrs

getMinionEngagedIdentity :: Minion -> IdentityId
getMinionEngagedIdentity = minionEngagedIdentity . toAttrs

getMinionPrintedHitPoints :: Minion -> HP Natural
getMinionPrintedHitPoints = minionHitPoints . toAttrs

minionAttackDetails :: Minion -> Maybe Attack
minionAttackDetails = minionAttacking . toAttrs

instance Entity Minion where
  type Id Minion = MinionId
  data Attrs Minion = MinionAttrs
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
    , minionDefensePriority :: DefensePriority
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Minion :: Type -> Type where
    MinionId :: Field Minion MinionId
    MinionCardDef :: Field Minion CardDef
    MinionDamage :: Field Minion Natural
    MinionHitPoints :: Field Minion (HP Natural)
    MinionScheme :: Field Minion Sch
    MinionAttack :: Field Minion Atk
    MinionEngagedIdentity :: Field Minion IdentityId
    MinionStunned :: Field Minion Bool
    MinionConfused :: Field Minion Bool
    MinionTough :: Field Minion Bool
    MinionAttacking :: Field Minion (Maybe Attack)
    MinionUpgrades :: Field Minion (HashSet UpgradeId)
    MinionAttachments :: Field Minion (HashSet AttachmentId)
    MinionDefensePriority :: Field Minion DefensePriority
  toId = minionId . toAttrs
  toAttrs (Minion a) = toMinionAttrs a

instance RunMessage Minion where
  runMessage msg (Minion a) = Minion <$> runMessage msg a

instance HasAbilities Minion where
  getAbilities (Minion a) = getAbilities a

instance HasModifiersFor Minion where
  getModifiersFor source target (Minion a) = getModifiersFor source target a

instance HasTraits Minion where
  getTraits m = do
    modifiers <- getModifiers m
    let traits = cdTraits $ getCardDef m
    pure $ foldr applyModifier traits modifiers
   where
    applyModifier (TraitModifier t) = HashSet.insert t
    applyModifier _ = id

instance IsSource Minion where
  toSource = MinionSource . toId

instance IsTarget Minion where
  toTarget = MinionTarget . toId

instance IsCard Minion where
  toCard = toCard . toAttrs

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs
minionRemainingHitPoints :: MinionAttrs -> Natural
minionRemainingHitPoints attrs =
  subtractNatural (minionDamage attrs) (unHp $ minionHitPoints attrs)

defensePriorityL :: Lens' MinionAttrs DefensePriority
defensePriorityL =
  lens minionDefensePriority $ \m x -> m { minionDefensePriority = x }

damageL :: Lens' MinionAttrs Natural
damageL = lens minionDamage $ \m x -> m { minionDamage = x }

attackingL :: Lens' MinionAttrs (Maybe Attack)
attackingL = lens minionAttacking $ \m x -> m { minionAttacking = x }

stunnedL :: Lens' MinionAttrs Bool
stunnedL = lens minionStunned $ \m x -> m { minionStunned = x }

confusedL :: Lens' MinionAttrs Bool
confusedL = lens minionConfused $ \m x -> m { minionConfused = x }

toughL :: Lens' MinionAttrs Bool
toughL = lens minionTough $ \m x -> m { minionTough = x }

upgradesL :: Lens' MinionAttrs (HashSet UpgradeId)
upgradesL = lens minionUpgrades $ \m x -> m { minionUpgrades = x }

attachmentsL :: Lens' MinionAttrs (HashSet AttachmentId)
attachmentsL = lens minionAttachments $ \m x -> m { minionAttachments = x }

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
    , minionDefensePriority = AnyDefense
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

getModifiedKeywords :: MonadGame env m => MinionAttrs -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

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
      keywords <- getModifiedKeywords attrs
      hitPoints <- getModifiedHitPoints attrs
      if (damageAmount damage + minionDamage attrs >= hitPoints)
        then do
          let overkill = damageAmount damage - (hitPoints - minionDamage attrs)
          when (overkill > 0 && damageOverkill damage) $ do
            villain <- selectJust ActiveVillain
            push $ VillainMessage
              villain
              (VillainDamaged source (toDamage overkill FromOverkill))

          pushAll
            [ CheckWindows
              [W.Window W.When $ W.DefeatedMinion (toId attrs) damage]
            , MinionMessage (toId attrs) MinionDefeated
            , CheckWindows
              [W.Window W.After $ W.DefeatedMinion (toId attrs) damage]
            ]
        else for_ keywords $ \case
          Retaliate n -> case damageSource damage of
            FromPlayerAttack ident -> push $ IdentityMessage
              ident
              (IdentityDamaged (toSource attrs) (toDamage n FromRetaliate))
            FromAllyAttack ident -> push $ AllyMessage
              ident
              (AllyDamaged (toSource attrs) (toDamage n FromRetaliate))
            _ -> pure ()
          _ -> pure ()
      pure $ attrs & damageL +~ damageAmount damage
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
  MinionBecomeTough -> pure $ attrs & toughL .~ True
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
      , DeclareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
      , MinionMessage (toId attrs) MinionAttacked
      , CheckWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
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
