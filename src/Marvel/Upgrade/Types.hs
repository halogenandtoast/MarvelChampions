module Marvel.Upgrade.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id hiding (UpgradeId)
import Marvel.Id as X (UpgradeId)
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Choice (chooseOrRunOne)
import Marvel.Query
import Marvel.Question
import Marvel.Queue
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
    , upgradeAttachedAlly :: Maybe AllyId
    , upgradeAttachedSideScheme :: Maybe SideSchemeId
    , upgradeAttachedMainScheme :: Maybe MainSchemeId
    , upgradeUses :: Natural
    , upgradeDiscardIfNoUses :: Bool
    , upgradeOwner :: IdentityId
    -- ^ whose deck it came from, and whose discard pile it goes to
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
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
  toId = upgradeId . toAttrs
  toAttrs (Upgrade a) = toUpgradeAttrs a

instance RunMessage Upgrade where
  runMessage msg (Upgrade a) = Upgrade <$> runMessage msg a

instance Exhaustable Upgrade where
  isExhausted = upgradeExhausted . toAttrs

instance IsRef Upgrade where
  toRef = UpgradeRef . toId

instance HasAbilities Upgrade where
  getAbilities (Upgrade a) = getAbilities a

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

type UpgradeCard a = CardBuilder (IdentityId, UpgradeId) a

attachedAllyL :: Lens' (Attrs Upgrade) (Maybe AllyId)
attachedAllyL =
  lens upgradeAttachedAlly $ \m x -> m {upgradeAttachedAlly = x}

attachedEnemyL :: Lens' (Attrs Upgrade) (Maybe EnemyId)
attachedEnemyL =
  lens upgradeAttachedEnemy $ \m x -> m {upgradeAttachedEnemy = x}

exhaustedL :: Lens' (Attrs Upgrade) Bool
exhaustedL = lens upgradeExhausted $ \m x -> m {upgradeExhausted = x}

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
            , upgradeAttachedAlly = Nothing
            , upgradeAttachedSideScheme = Nothing
            , upgradeAttachedMainScheme = Nothing
            , upgradeUses = 0
            , upgradeDiscardIfNoUses = False
            , upgradeOwner = ident
            }
    }

damageChoice :: Attrs Upgrade -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid ->
    TargetLabel
      (toRef vid)
      [DamageEnemy (toRef vid) (toSource attrs) dmg]
  EnemyMinionId vid ->
    TargetLabel
      (toRef vid)
      [DamageEnemy (toRef vid) (toSource attrs) dmg]

thwartGuard ::
  (HasGame m, HasQueue m, IsUpgrade u) =>
  u ->
  m u ->
  m u
thwartGuard u f = do
  let ident = upgradeController (toUpgradeAttrs u)
  confused <- selectAny (IdentityWithId ident <> ConfusedIdentity)
  if confused
    then do
      push (IdentityMessage ident IdentityRemoveConfused)
      pure u
    else f

-- | An attack: a stunned player only removes the stun (the cost is still paid).
attackGuard ::
  (HasGame m, HasQueue m, IsUpgrade u) =>
  IdentityId ->
  u ->
  m u ->
  m u
attackGuard ident u f = do
  stunned <- selectAny (IdentityWithId ident <> StunnedIdentity)
  if stunned
    then do
      push (IdentityMessage ident IdentityRemoveStunned)
      pure u
    else f

thwartChoice :: Attrs Upgrade -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid ->
    TargetLabel
      (toRef vid)
      [ThwartScheme (toRef vid) (toSource attrs) thw]
  SchemeSideSchemeId sid ->
    TargetLabel
      (toRef sid)
      [ThwartScheme (toRef sid) (toSource attrs) thw]

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
  runMessage msg a = case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      PlayedUpgrade -> case cdLimit (upgradeCardDef a) of
        -- "play under any player's control": its player chooses whose, among those
        -- without one
        Just (MaxPerPlayerUnderAnyControl n) -> do
          players <- getPlayers
          open <- flip filterM players \p ->
            (< n) . fromIntegral . length . filter (/= upgradeId a)
              <$> selectList (UpgradeNamed (cdName $ upgradeCardDef a) <> UpgradeControlledBy (IdentityWithId p))
          underControlOf open
        Just UnderAnyControl -> underControlOf =<< getPlayers
        _ ->
          a
            <$ push
              (IdentityMessage (upgradeController a) $ UpgradeCreated (upgradeId a))
       where
        underControlOf open = do
          chooseOrRunOne (upgradeController a)
            [ TargetLabel (IdentityRef p) [Run [UpgradeMessage (upgradeId a) (UpgradeControlledByPlayer p)]]
            | p <- open
            ]
          pure a
      UpgradeControlledByPlayer p -> do
        push $ IdentityMessage p $ UpgradeCreated (upgradeId a)
        pure a {upgradeController = p}
      ReadiedUpgrade -> do
        pure $ a & exhaustedL .~ False
      AddUpgradeUses n -> do
        pure $ a & usesL +~ n
      SpendUpgradeUse -> do
        when
          (upgradeUses a == 1 && upgradeDiscardIfNoUses a)
          (push $ RemoveFromPlay (toTarget a))
        pure $ a & usesL -~ 1
      ExhaustedUpgrade -> do
        pure $ a & exhaustedL .~ True
      UpgradeAttachedToEnemy enemyId -> do
        case enemyId of
          EnemyMinionId minionId ->
            push (MinionMessage minionId $ AttachedUpgradeToMinion (upgradeId a))
          EnemyVillainId villainId ->
            push (VillainMessage villainId $ AttachedUpgradeToVillain (upgradeId a))
        pure $ a & attachedEnemyL ?~ enemyId
      UpgradeAttachedToAlly allyId -> do
        push (AllyMessage allyId $ AttachedUpgradeToAlly (upgradeId a))
        pure $ a & attachedAllyL ?~ allyId
      UpgradeAttachedToSideScheme sideSchemeId ->
        pure a {upgradeAttachedSideScheme = Just sideSchemeId}
      UpgradeAttachedToMainScheme mainSchemeId ->
        pure a {upgradeAttachedMainScheme = Just mainSchemeId}
      -- leaving play discards it
      DiscardUpgrade -> do
        push $ RemoveFromPlay (toTarget a)
        pure a
    _ -> pure a
