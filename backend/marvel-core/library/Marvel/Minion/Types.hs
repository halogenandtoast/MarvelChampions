module Marvel.Minion.Types where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability.Types
import Marvel.Attack
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id hiding (MinionId)
import Marvel.Id as X (MinionId)
import Marvel.Keyword
import Marvel.Message hiding (
  MinionConfused,
  MinionEngagedIdentity,
  MinionStunned,
 )
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait

data Minion = forall a. (IsMinion a) => Minion a

instance Show Minion where
  show (Minion a) = show a

instance ToJSON Minion where
  toJSON (Minion a) = toJSON a

instance Eq Minion where
  Minion (a :: a) == Minion (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeMinionCard = forall a. (IsMinion a) => SomeMinionCard (MinionCard a)

liftMinionCard :: (forall a. MinionCard a -> b) -> SomeMinionCard -> b
liftMinionCard f (SomeMinionCard a) = f a

someMinionCardCode :: SomeMinionCard -> CardCode
someMinionCardCode = liftMinionCard cbCardCode

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, RunMessage a, HasAbilities a) => IsMinion a where
  toMinionAttrs :: a -> Attrs Minion
  default toMinionAttrs :: (Coercible a (Attrs Minion)) => a -> Attrs Minion
  toMinionAttrs = coerce

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
    , minionOriginalCardDef :: CardDef
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
  field fld m =
    let MinionAttrs {..} = toAttrs m
     in case fld of
          MinionId -> minionId
          MinionCardDef -> minionCardDef
          MinionDamage -> minionDamage
          MinionHitPoints -> minionHitPoints
          MinionScheme -> minionScheme
          MinionAttack -> minionAttack
          MinionEngagedIdentity -> minionEngagedIdentity
          MinionStunned -> minionStunned
          MinionConfused -> minionConfused
          MinionTough -> minionTough
          MinionAttacking -> minionAttacking
          MinionUpgrades -> minionUpgrades
          MinionAttachments -> minionAttachments
          MinionDefensePriority -> minionDefensePriority
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

instance IsRef Minion where
  toRef = MinionRef . toId

instance IsCard Minion where
  toCard = toCard . toAttrs

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs

minionRemainingHitPoints :: Attrs Minion -> Natural
minionRemainingHitPoints attrs =
  subtractNatural (minionDamage attrs) (unHp $ minionHitPoints attrs)

defensePriorityL :: Lens' (Attrs Minion) DefensePriority
defensePriorityL =
  lens minionDefensePriority $ \m x -> m {minionDefensePriority = x}

damageL :: Lens' (Attrs Minion) Natural
damageL = lens minionDamage $ \m x -> m {minionDamage = x}

attackingL :: Lens' (Attrs Minion) (Maybe Attack)
attackingL = lens minionAttacking $ \m x -> m {minionAttacking = x}

stunnedL :: Lens' (Attrs Minion) Bool
stunnedL = lens minionStunned $ \m x -> m {minionStunned = x}

confusedL :: Lens' (Attrs Minion) Bool
confusedL = lens minionConfused $ \m x -> m {minionConfused = x}

toughL :: Lens' (Attrs Minion) Bool
toughL = lens minionTough $ \m x -> m {minionTough = x}

upgradesL :: Lens' (Attrs Minion) (HashSet UpgradeId)
upgradesL = lens minionUpgrades $ \m x -> m {minionUpgrades = x}

attachmentsL :: Lens' (Attrs Minion) (HashSet AttachmentId)
attachmentsL = lens minionAttachments $ \m x -> m {minionAttachments = x}

instance HasCardCode (Attrs Minion) where
  toCardCode = toCardCode . minionCardDef

minionWith ::
  (Attrs Minion -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP Natural ->
  (Attrs Minion -> Attrs Minion) ->
  CardBuilder (IdentityId, MinionId) a
minionWith f cardDef sch atk hp g = minion (f . g) cardDef sch atk hp

minion ::
  (Attrs Minion -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP Natural ->
  CardBuilder (IdentityId, MinionId) a
minion f cardDef sch atk hp =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, mid) ->
        f $
          MinionAttrs
            { minionId = mid
            , minionCardDef = cardDef
            , minionOriginalCardDef = cardDef
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

instance IsRef (Attrs Minion) where
  toRef = MinionRef . minionId

instance IsCard (Attrs Minion) where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unMinionId $ minionId a
        , ecCardDef = getCardDef a
        }

instance HasCardDef (Attrs Minion) where
  getCardDef = minionCardDef

getModifiedKeywords :: (HasGame m) => Attrs Minion -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

getModifiedHitPoints :: (HasGame m) => Attrs Minion -> m Natural
getModifiedHitPoints attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unHp $ minionHitPoints attrs) modifiers
 where
  applyModifier (HitPointModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedAttack :: (HasGame m) => Attrs Minion -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ minionAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

toEnemyId :: Attrs Minion -> EnemyId
toEnemyId = EnemyMinionId . minionId
