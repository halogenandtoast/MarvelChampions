module Marvel.Ally.Types
  ( module Marvel.Ally.Types
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Modifier as X
import Marvel.Queue as X
import Marvel.Source as X
import Marvel.Stats as X
import Marvel.Target as X

import Data.Typeable
import Marvel.Ability.Type
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id hiding (AllyId)
import Marvel.Id qualified as Id
import Marvel.Message hiding (AllyStunned)
import Marvel.Question hiding (AllyAttack, AllyThwart)
import Marvel.Window qualified as W
import Text.Show qualified

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, HasAbilities a, RunMessage a, IsSource a) => IsAlly a where
  toAllyAttrs :: a -> Attrs Ally
  default toAllyAttrs :: Coercible a (Attrs Ally) => a -> Attrs Ally
  toAllyAttrs = coerce

type AllyCard a = CardBuilder (IdentityId, Id.AllyId) a

data Ally = forall a . IsAlly a => Ally a

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a

instance Eq Ally where
  Ally (a :: a) == Ally (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAllyCard = forall a . IsAlly a => SomeAllyCard (AllyCard a)

liftAllyCard :: (forall a . AllyCard a -> b) -> SomeAllyCard -> b
liftAllyCard f (SomeAllyCard a) = f a

someAllyCardCode :: SomeAllyCard -> CardCode
someAllyCardCode = liftAllyCard cbCardCode

upgradesL :: Lens' (Attrs Ally) (HashSet UpgradeId)
upgradesL = lens allyUpgrades $ \m x -> m { allyUpgrades = x }

countersL :: Lens' (Attrs Ally) Natural
countersL = lens allyCounters $ \m x -> m { allyCounters = x }

damageL :: Lens' (Attrs Ally) Natural
damageL = lens allyDamage $ \m x -> m { allyDamage = x }

toughL :: Lens' (Attrs Ally) Bool
toughL = lens allyTough $ \m x -> m { allyTough = x }

confusedL :: Lens' (Attrs Ally) Bool
confusedL = lens allyConfused $ \m x -> m { allyConfused = x }

stunnedL :: Lens' (Attrs Ally) Bool
stunnedL = lens allyStunned $ \m x -> m { allyStunned = x }

exhaustedL :: Lens' (Attrs Ally) Bool
exhaustedL = lens allyExhausted $ \m x -> m { allyExhausted = x }

instance HasController (Attrs Ally) where
  controller = allyController

controllerMessage :: IsAlly a => a -> IdentityMessage -> Message
controllerMessage a = IdentityMessage (allyController $ toAllyAttrs a)

instance HasCardCode (Attrs Ally) where
  toCardCode = toCardCode . allyCardDef

instance HasCardDef (Attrs Ally) where
  getCardDef = allyCardDef

allyWith
  :: (Attrs Ally -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> (Attrs Ally -> Attrs Ally)
  -> CardBuilder (IdentityId, Id.AllyId) a
allyWith f cardDef thwPair atkPair hp g =
  ally (f . g) cardDef thwPair atkPair hp

ally
  :: (Attrs Ally -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> CardBuilder (IdentityId, Id.AllyId) a
ally f cardDef (thw, thwConsequentialDamage) (atk, atkConsequentialDamage) hp =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, aid) -> f $ AllyAttrs
      { allyId = aid
      , allyCardDef = cardDef
      , allyDamage = 0
      , allyAttack = atk
      , allyAttackConsequentialDamage = atkConsequentialDamage
      , allyThwart = thw
      , allyThwartConsequentialDamage = thwConsequentialDamage
      , allyController = ident
      , allyHitPoints = hp
      , allyExhausted = False
      , allyStunned = False
      , allyConfused = False
      , allyTough = False
      , allyCounters = 0
      , allyUpgrades = mempty
      }
    }

instance IsSource (Attrs Ally) where
  toSource = AllySource . allyId

instance IsTarget (Attrs Ally) where
  toTarget = AllyTarget . allyId

getModifiedAttack :: MonadGame env m => Attrs Ally -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ allyAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: MonadGame env m => Attrs Ally -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ allyThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: Attrs Ally -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [ DamageEnemy (VillainTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [W.Window W.After $ W.AllyAttack (allyId attrs) (EnemyVillainId vid)]
      ]
    ]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [ DamageEnemy (MinionTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [W.Window W.After $ W.AllyAttack (allyId attrs) (EnemyMinionId vid)]
      ]
    ]

thwartChoice :: Attrs Ally -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (allyId attrs) (SchemeMainSchemeId vid)
          ]
      ]
    ]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (allyId attrs) (SchemeSideSchemeId sid)
          ]
      ]
    ]

stunChoice :: IsAlly a => a -> EnemyId -> Choice
stunChoice a = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [Stun (VillainTarget vid) (AllySource $ allyId $ toAllyAttrs a)]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [Stun (MinionTarget vid) (AllySource $ allyId $ toAllyAttrs a)]

instance IsCard (Attrs Ally) where
  toCard a = PlayerCard $ MkPlayerCard
    { pcCardId = CardId $ unAllyId (allyId a)
    , pcCardDef = allyCardDef a
    , pcOwner = Just (allyController a)
    , pcController = Just (allyController a)
    }

instance Entity Ally where
  type Id Ally = Id.AllyId
  data Attrs Ally = AllyAttrs
    { allyId :: Id.AllyId
    , allyCardDef :: CardDef
    , allyDamage :: Natural
    , allyHitPoints :: HP Natural
    , allyThwart :: Thw
    , allyThwartConsequentialDamage :: Natural
    , allyAttack :: Atk
    , allyAttackConsequentialDamage :: Natural
    , allyController :: IdentityId
    , allyExhausted :: Bool
    , allyCounters :: Natural
    , allyUpgrades :: HashSet UpgradeId
    , allyStunned :: Bool
    , allyConfused :: Bool
    , allyTough :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Ally :: Type -> Type where
    AllyId :: Field Ally Id.AllyId
    AllyCardDef :: Field Ally CardDef
    AllyDamage :: Field Ally Natural
    AllyHitPoints :: Field Ally (HP Natural)
    AllyThwart :: Field Ally Thw
    AllyThwartConsequentialDamage :: Field Ally Natural
    AllyAttack :: Field Ally Atk
    AllyAttackConsequentialDamage :: Field Ally Natural
    AllyController :: Field Ally IdentityId
    AllyExhausted :: Field Ally Bool
    AllyCounters :: Field Ally Natural
    AllyUpgrades :: Field Ally (HashSet UpgradeId)
    AllyStunned :: Field Ally Bool
    AllyConfused :: Field Ally Bool
    AllyTough :: Field Ally Bool
  toId = allyId . toAttrs
  toAttrs (Ally a) = toAllyAttrs a
  field fld a =
    let AllyAttrs {..} = toAttrs a
    in
      case fld of
        AllyId -> allyId
        AllyCardDef -> allyCardDef
        AllyDamage -> allyDamage
        AllyHitPoints -> allyHitPoints
        AllyThwart -> allyThwart
        AllyThwartConsequentialDamage -> allyThwartConsequentialDamage
        AllyAttack -> allyAttack
        AllyAttackConsequentialDamage -> allyAttackConsequentialDamage
        AllyController -> allyController
        AllyExhausted -> allyExhausted
        AllyCounters -> allyCounters
        AllyUpgrades -> allyUpgrades
        AllyStunned -> allyStunned
        AllyConfused -> allyConfused
        AllyTough -> allyTough

instance RunMessage Ally where
  runMessage msg (Ally a) = Ally <$> runMessage msg a

instance Exhaustable Ally where
  isExhausted = allyExhausted . toAttrs

instance IsCard Ally where
  toCard = toCard . toAttrs

instance IsSource Ally where
  toSource = AllySource . toId

getAllyController :: Ally -> IdentityId
getAllyController = allyController . toAttrs

getAllyUses :: Ally -> Natural
getAllyUses = allyCounters . toAttrs

getAllyDamage :: Ally -> Natural
getAllyDamage = allyDamage . toAttrs

instance HasCardDef Ally where
  getCardDef = getCardDef . toAttrs

instance HasModifiersFor Ally where
  getModifiersFor source target (Ally a) = getModifiersFor source target a
