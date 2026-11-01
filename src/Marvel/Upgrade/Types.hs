module Marvel.Upgrade.Types where

import Marvel.Prelude

import Data.Typeable
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Ability (limitedWindowAbility)
import Marvel.Ability.Types
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Criteria (Criteria (SourceHasKeyword))
import Marvel.Question (Choice (DiscardTarget))
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id hiding (UpgradeId)
import Marvel.Id as X (UpgradeId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Keyword (Keyword (Permanent, Temporary))
import Marvel.Query
import Marvel.Ref

data Upgrade = forall a. (IsUpgrade a) => Upgrade a

instance Show Upgrade where
  show (Upgrade a) = show a

instance Eq Upgrade where
  Upgrade (a :: a) == Upgrade (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Upgrade where
  toJSON (Upgrade a) = toJSON a

data SomeUpgradeCard
  = forall a.
    (IsUpgrade a) =>
    SomeUpgradeCard
      (UpgradeCard a)

liftUpgradeCard :: (forall a. UpgradeCard a -> b) -> SomeUpgradeCard -> b
liftUpgradeCard f (SomeUpgradeCard a) = f a

someUpgradeCardCode :: SomeUpgradeCard -> CardCode
someUpgradeCardCode = liftUpgradeCard toCardCode

instance Entity Upgrade where
  type Id Upgrade = UpgradeId
  data Attrs Upgrade = UpgradeAttrs
    { upgradeId :: UpgradeId
    , upgradeCardDef :: CardDef
    , upgradeController :: IdentityId
    , upgradeExhausted :: Bool
    , upgradeAttachedEnemy :: Maybe EnemyId
    , upgradeAttachedIdentity :: Maybe IdentityId
    , upgradeAttachedAlly :: Maybe AllyId
    , upgradeAttachedSideScheme :: Maybe SideSchemeId
    , upgradeAttachedMainScheme :: Maybe MainSchemeId
    , upgradeUses :: Natural
    , upgradeDiscardIfNoUses :: Bool
    , upgradeOwner :: IdentityId
    -- ^ whose deck it came from, and whose discard pile it goes to
    , upgradeHeldCards :: [PlayerCard]
    -- ^ cards attached faceup to it, its controller may play as if in hand (Hawkeye's
    -- Quiver); discarded as it leaves play
    , upgradeFaceDown :: Bool
    -- ^ facedown, out of play for its text (Spectrum's energy forms)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
  data Field Upgrade :: Type -> Type where
    UpgradeId :: Field Upgrade UpgradeId
    UpgradeCardDef :: Field Upgrade CardDef
    UpgradeController :: Field Upgrade IdentityId
    UpgradeExhausted :: Field Upgrade Bool
    UpgradeAttachedEnemy :: Field Upgrade (Maybe EnemyId)
    UpgradeAttachedAlly :: Field Upgrade (Maybe AllyId)
    UpgradeAttachedSideScheme :: Field Upgrade (Maybe SideSchemeId)
    UpgradeAttachedMainScheme :: Field Upgrade (Maybe MainSchemeId)
    UpgradeUses :: Field Upgrade Natural
    UpgradeDiscardIfNoUses :: Field Upgrade Bool
    UpgradeHeldCards :: Field Upgrade [PlayerCard]
    UpgradeFaceDown :: Field Upgrade Bool
  field fld u =
    let UpgradeAttrs {..} = toAttrs u
     in case fld of
          UpgradeId -> upgradeId
          UpgradeCardDef -> upgradeCardDef
          UpgradeController -> upgradeController
          UpgradeExhausted -> upgradeExhausted
          UpgradeAttachedEnemy -> upgradeAttachedEnemy
          UpgradeAttachedAlly -> upgradeAttachedAlly
          UpgradeAttachedSideScheme -> upgradeAttachedSideScheme
          UpgradeAttachedMainScheme -> upgradeAttachedMainScheme
          UpgradeUses -> upgradeUses
          UpgradeDiscardIfNoUses -> upgradeDiscardIfNoUses
          UpgradeHeldCards -> upgradeHeldCards
          UpgradeFaceDown -> upgradeFaceDown
  toId = upgradeId . toAttrs
  toAttrs (Upgrade a) = toUpgradeAttrs a

instance RunMessage Upgrade where
  runMessage msg (Upgrade a) = Upgrade <$> runMessage msg a

instance Exhaustable Upgrade where
  isExhausted = upgradeExhausted . toAttrs

instance IsRef Upgrade where
  toRef = UpgradeRef . toId

instance HasAbilities Upgrade where
  getAbilities (Upgrade a) = getAbilities a <>
    [ limitedWindowAbility (toUpgradeAttrs a) 998 W.RoundEnds ForcedInterrupt (SourceHasKeyword Temporary) NoCost
        (DiscardTarget $ toTarget $ toUpgradeAttrs a)
    | Temporary `elem` cdKeywords (getCardDef $ toUpgradeAttrs a)
    ]

instance HasCardDef Upgrade where
  getCardDef = getCardDef . toAttrs

instance IsCard Upgrade where
  toCard = toCard . toAttrs

getUpgradeController :: Upgrade -> IdentityId
getUpgradeController = upgradeController . toAttrs

getUpgradeUses :: Upgrade -> Natural
getUpgradeUses = upgradeUses . toAttrs

instance HasModifiersFor Upgrade where
  getModifiersFor source target (Upgrade a) = getModifiersFor source target a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a, HasAbilities a, HasModifiersFor a) => IsUpgrade a where
  toUpgradeAttrs :: a -> Attrs Upgrade
  default toUpgradeAttrs :: (Coercible a (Attrs Upgrade)) => a -> Attrs Upgrade
  toUpgradeAttrs = coerce

  -- | What it is played on, chosen before its cost is paid: what it attaches to, or
  -- the player who takes control of it. Nothing for an upgrade played on neither.
  upgradeTargets :: (HasGame m) => a -> m (Maybe [Target])
  upgradeTargets = controlTargets . toUpgradeAttrs

  -- | Whether the upgrade's Special can change the game state now. Cards without
  -- a Special default to False; status-card replacement can make one meaningful
  -- even without a target (stun/confusion replaces an attack/thwart attempt).
  upgradeSpecialCanResolve :: (HasGame m) => a -> m Bool
  upgradeSpecialCanResolve _ = pure False

-- | "Play under any player's control": its player chooses whose, among those who may.
controlTargets :: (HasGame m) => Attrs Upgrade -> m (Maybe [Target])
controlTargets a = case cdLimit (upgradeCardDef a) of
  Just (MaxPerPlayerUnderAnyControl n) -> do
    players <- getPlayers
    open <- flip filterM players \p ->
      (< n) . fromIntegral . length
        <$> selectList (UpgradeNamed (cdName $ upgradeCardDef a) <> UpgradeControlledBy (IdentityWithId p))
    pure $ Just $ map toTarget open
  Just UnderAnyControl -> Just . map toTarget <$> getPlayers
  _ -> pure Nothing

type UpgradeCard a = CardBuilder (IdentityId, UpgradeId) a

attachedAllyL :: Lens' (Attrs Upgrade) (Maybe AllyId)
attachedAllyL =
  lens upgradeAttachedAlly $ \m x -> m {upgradeAttachedAlly = x}

attachedEnemyL :: Lens' (Attrs Upgrade) (Maybe EnemyId)
attachedEnemyL =
  lens upgradeAttachedEnemy $ \m x -> m {upgradeAttachedEnemy = x}

exhaustedL :: Lens' (Attrs Upgrade) Bool
exhaustedL = lens upgradeExhausted $ \m x -> m {upgradeExhausted = x}

heldCardsL :: Lens' (Attrs Upgrade) [PlayerCard]
heldCardsL = lens upgradeHeldCards $ \m x -> m {upgradeHeldCards = x}

usesL :: Lens' (Attrs Upgrade) Natural
usesL = lens upgradeUses $ \m x -> m {upgradeUses = x}

discardIfNoUsesL :: Lens' (Attrs Upgrade) Bool
discardIfNoUsesL =
  lens upgradeDiscardIfNoUses $ \m x -> m {upgradeDiscardIfNoUses = x}

instance HasCardCode (Attrs Upgrade) where
  toCardCode = toCardCode . upgradeCardDef

upgradeWith ::
  (Attrs Upgrade -> a) ->
  CardDef ->
  (Attrs Upgrade -> Attrs Upgrade) ->
  CardBuilder (IdentityId, UpgradeId) a
upgradeWith f cardDef g = upgrade (f . g) cardDef

upgrade ::
  (Attrs Upgrade -> a) -> CardDef -> CardBuilder (IdentityId, UpgradeId) a
upgrade f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (ident, mid) ->
        f $
          UpgradeAttrs
            { upgradeId = mid
            , upgradeCardDef = cardDef
            , upgradeController = ident
            , upgradeExhausted = False
            , upgradeAttachedEnemy = Nothing
            , upgradeAttachedIdentity = Nothing
            , upgradeAttachedAlly = Nothing
            , upgradeAttachedSideScheme = Nothing
            , upgradeAttachedMainScheme = Nothing
            , upgradeUses = 0
            , upgradeDiscardIfNoUses = False
            , upgradeOwner = ident
            , upgradeHeldCards = []
            , upgradeFaceDown = False
            }
    }

thwartGuard ::
  (ReverseQueue m, IsUpgrade u) =>
  u ->
  m u ->
  m u
thwartGuard u f = do
  let ident = upgradeController (toUpgradeAttrs u)
  confused <- selectAny (IdentityWithId ident <> ConfusedIdentity)
  if confused
    then do
      removeConfused ident
      pure u
    else f

-- | An attack: a stunned player only removes the stun (the cost is still paid).
attackGuard ::
  (ReverseQueue m, IsUpgrade u) =>
  IdentityId ->
  u ->
  m u ->
  m u
attackGuard ident u f = do
  stunned <- selectAny (IdentityWithId ident <> StunnedIdentity)
  if stunned
    then do
      removeStun ident
      pure u
    else f

instance IsRef (Attrs Upgrade) where
  toRef = UpgradeRef . upgradeId

instance HasCardDef (Attrs Upgrade) where
  getCardDef = upgradeCardDef

instance IsCard (Attrs Upgrade) where
  toCard a =
    PlayerCard $
      MkPlayerCard
        { pcCardId = CardId $ unUpgradeId $ upgradeId a
        , pcCardDef = getCardDef a
        , pcOwner = Just (upgradeOwner a)
        , pcController = Just (upgradeController a)
        }

instance RunMessage (Attrs Upgrade) where
  runMessage msg a = runQueueT $ case msg of
    RemovePlayerCardFromHeldZones cid -> pure a {upgradeHeldCards = filter ((/= cid) . pcCardId) (upgradeHeldCards a)}
    CardSpecific (isTarget a -> True) "attachTouched" (fromJSON -> Success host) -> do
      let uid = upgradeId a
      case host of
        IdentityCharacter _ -> pure ()
        _ -> upgradeAttachedTo host uid
      pure a {upgradeAttachedEnemy = case host of
                MinionCharacter mid -> Just $ EnemyMinionId mid
                VillainCharacter vid -> Just $ EnemyVillainId vid
                _ -> Nothing
             , upgradeAttachedAlly = case host of AllyCharacter aid -> Just aid; _ -> Nothing
             , upgradeAttachedIdentity = case host of IdentityCharacter you -> Just you; _ -> Nothing}
    RemoveCounters target n | target == toRef a -> do
      when (upgradeUses a > 0 && n >= upgradeUses a && upgradeDiscardIfNoUses a) $ removeFromPlay a
      pure $ a & usesL %~ subtractNatural n
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      UpgradeDetached -> pure a {upgradeAttachedEnemy = Nothing, upgradeAttachedIdentity = Nothing, upgradeAttachedAlly = Nothing,
        upgradeAttachedSideScheme = Nothing, upgradeAttachedMainScheme = Nothing}
      PlayedUpgrade mTarget -> do
        case mTarget of
          Just (IdentityRef p) -> upgradeStep (upgradeId a) (UpgradeControlledByPlayer p)
          -- Register only the host's controller, so the payer never briefly counts
          -- this restricted upgrade against their own limit.
          Just (AllyRef aid) -> upgradeStep (upgradeId a) (UpgradeAttachedToAlly aid)
          Just target -> do
            upgradeCreated (upgradeController a) (upgradeId a)
            upgradeStep (upgradeId a) (attachedTo target)
          Nothing -> upgradeCreated (upgradeController a) (upgradeId a)
        pure a
       where
        attachedTo = \case
          AllyRef allyId -> UpgradeAttachedToAlly allyId
          MinionRef minionId -> UpgradeAttachedToEnemy (EnemyMinionId minionId)
          VillainRef villainId -> UpgradeAttachedToEnemy (EnemyVillainId villainId)
          EnemyRef enemyId -> UpgradeAttachedToEnemy enemyId
          SideSchemeRef sideSchemeId -> UpgradeAttachedToSideScheme sideSchemeId
          SchemeRef (SchemeMainSchemeId mainSchemeId) -> UpgradeAttachedToMainScheme mainSchemeId
          MainSchemeRef mainSchemeId -> UpgradeAttachedToMainScheme mainSchemeId
          target -> error $ "an upgrade cannot attach to " <> show target
      UpgradeControlledByPlayer p -> do
        when (p /= upgradeController a) $ upgradeControlLost (upgradeController a) (upgradeId a)
        upgradeCreated p (upgradeId a)
        pure a {upgradeController = p}
      ReadiedUpgrade -> do
        readies <- notElem CannotReady <$> getModifiers a
        pure $ a & exhaustedL %~ (&& not readies)
      AddUpgradeUses n -> do
        pure $ a & usesL +~ n
      SpendUpgradeUse -> do
        when (upgradeUses a == 1 && upgradeDiscardIfNoUses a) $ removeFromPlay a
        pure $ a & usesL -~ 1
      ExhaustedUpgrade -> do
        pure $ a & exhaustedL .~ True
      UpgradeOwnedBy owner -> pure a {upgradeOwner = owner}
      FlipUpgradeFaceUp -> pure a {upgradeFaceDown = False}
      FlipUpgradeFaceDown -> pure a {upgradeFaceDown = True}
      UpgradeAttachedToEnemy enemyId -> do
        upgradeAttachedTo enemyId (upgradeId a)
        pure $ a & attachedEnemyL ?~ enemyId
      UpgradeAttachedToIdentity you -> pure a {upgradeAttachedIdentity = Just you}
      UpgradeAttachedToAlly allyId -> do
        hostController <- fromMaybe (upgradeController a) <$> getSourceController (AllyRef allyId)
        when (hostController /= upgradeController a) $ upgradeControlLost (upgradeController a) (upgradeId a)
        -- Attach before the new controller checks restricted upgrades; discarding
        -- one during that check must not enqueue a later phantom attachment.
        upgradeAttachedTo allyId (upgradeId a)
        upgradeCreated hostController (upgradeId a)
        let priorEnemy = case upgradeAttachedEnemy a of
              Just enemy@(EnemyMinionId mid) | unMinionId mid == unAllyId allyId -> Just enemy
              _ -> Nothing
        pure a {upgradeAttachedAlly = Just allyId, upgradeAttachedEnemy = priorEnemy, upgradeController = hostController}
      UpgradeHostBecameMinion mid -> do
        upgradeAttachedTo mid (upgradeId a)
        pure a {upgradeAttachedEnemy = Just (EnemyMinionId mid), upgradeAttachedAlly = Just (AllyId $ unMinionId mid)}
      UpgradeAttachedToSideScheme sideSchemeId ->
        pure a {upgradeAttachedSideScheme = Just sideSchemeId}
      UpgradeAttachedToMainScheme mainSchemeId ->
        pure a {upgradeAttachedMainScheme = Just mainSchemeId}
      -- leaving play discards it
      DiscardUpgrade -> do
        removeFromPlay a
        pure a
    -- A permanent attachment returns to its controller when its host leaves play.
    msg' | Permanent `elem` cdKeywords (getCardDef a), Just host <- leavingHost msg', host `elem` attachedHosts -> do
      detachUpgrade (upgradeId a)
      pure a
    -- a held card played leaves it
    PutCardIntoPlay _ card _ _ _ -> pure $ a & heldCardsL %~ filter (/= card)
    _ -> pure a
   where
    attachedHosts = catMaybes [enemyRef <$> upgradeAttachedEnemy a, AllyRef <$> upgradeAttachedAlly a,
      SideSchemeRef <$> upgradeAttachedSideScheme a, MainSchemeRef <$> upgradeAttachedMainScheme a]
    leavingHost = \case
      RemoveFromPlay target -> Just target
      RemoveFromGame target -> Just target
      ReturnToHand target -> Just target
      AddToVictoryDisplay target -> Just target
      _ -> Nothing

instance FromJSON (Attrs Upgrade) where
  parseJSON = withObject "UpgradeAttrs" $ \o -> genericParseJSON defaultOptions (Object $ KeyMap.insertWith (\_ old -> old) "upgradeAttachedIdentity" (toJSON (Nothing :: Maybe IdentityId)) o)
