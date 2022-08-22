module Marvel.Ally.Types
  ( module Marvel.Ally.Types
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Message as X
import Marvel.Modifier as X
import Marvel.Question as X
import Marvel.Queue as X
import Marvel.Source as X
import Marvel.Stats as X
import Marvel.Target as X

import Data.Typeable
import Marvel.Ability.Type
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Window qualified as W
import Text.Show qualified

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ AllyAttrs, EntityId a ~ AllyId, HasModifiersFor a, HasAbilities a, RunMessage a, IsSource a) => IsAlly a

type AllyCard a = CardBuilder (IdentityId, AllyId) a

data Ally = forall a . IsAlly a => Ally a

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a

instance Eq Ally where
  (Ally (a :: a)) == (Ally (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAllyCard = forall a . IsAlly a => SomeAllyCard (AllyCard a)

liftAllyCard :: (forall a . AllyCard a -> b) -> SomeAllyCard -> b
liftAllyCard f (SomeAllyCard a) = f a

someAllyCardCode :: SomeAllyCard -> CardCode
someAllyCardCode = liftAllyCard cbCardCode

data AllyAttrs = AllyAttrs
  { allyId :: AllyId
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

upgradesL :: Lens' AllyAttrs (HashSet UpgradeId)
upgradesL = lens allyUpgrades $ \m x -> m { allyUpgrades = x }

countersL :: Lens' AllyAttrs Natural
countersL = lens allyCounters $ \m x -> m { allyCounters = x }

damageL :: Lens' AllyAttrs Natural
damageL = lens allyDamage $ \m x -> m { allyDamage = x }

toughL :: Lens' AllyAttrs Bool
toughL = lens allyTough $ \m x -> m { allyTough = x }

confusedL :: Lens' AllyAttrs Bool
confusedL = lens allyConfused $ \m x -> m { allyConfused = x }

stunnedL :: Lens' AllyAttrs Bool
stunnedL = lens allyStunned $ \m x -> m { allyStunned = x }

exhaustedL :: Lens' AllyAttrs Bool
exhaustedL = lens allyExhausted $ \m x -> m { allyExhausted = x }

instance HasController AllyAttrs where
  controller = allyController

controllerMessage
  :: (EntityAttrs a ~ AllyAttrs, Entity a) => a -> IdentityMessage -> Message
controllerMessage a = IdentityMessage (allyController $ toAttrs a)

instance HasCardCode AllyAttrs where
  toCardCode = toCardCode . allyCardDef

instance HasCardDef AllyAttrs where
  getCardDef = allyCardDef

allyWith
  :: (AllyAttrs -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> (AllyAttrs -> AllyAttrs)
  -> CardBuilder (IdentityId, AllyId) a
allyWith f cardDef thwPair atkPair hp g =
  ally (f . g) cardDef thwPair atkPair hp

ally
  :: (AllyAttrs -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> CardBuilder (IdentityId, AllyId) a
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

instance Entity AllyAttrs where
  type EntityId AllyAttrs = AllyId
  type EntityAttrs AllyAttrs = AllyAttrs
  toId = allyId
  toAttrs = id

instance IsSource AllyAttrs where
  toSource = AllySource . toId

instance IsTarget AllyAttrs where
  toTarget = AllyTarget . toId

getModifiedAttack :: MonadGame env m => AllyAttrs -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ allyAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: MonadGame env m => AllyAttrs -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ allyThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: AllyAttrs -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [ DamageEnemy (VillainTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [W.Window W.After $ W.AllyAttack (toId attrs) (EnemyVillainId vid)]
      ]
    ]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [ DamageEnemy (MinionTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [W.Window W.After $ W.AllyAttack (toId attrs) (EnemyMinionId vid)]
      ]
    ]

thwartChoice :: AllyAttrs -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (toId attrs) (SchemeMainSchemeId vid)
          ]
      ]
    ]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (toId attrs) (SchemeSideSchemeId sid)
          ]
      ]
    ]

stunChoice :: (Entity a, EntityId a ~ AllyId) => a -> EnemyId -> Choice
stunChoice a = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [Stun (VillainTarget vid) (AllySource $ toId a)]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [Stun (MinionTarget vid) (AllySource $ toId a)]

instance IsCard AllyAttrs where
  toCard a = PlayerCard $ MkPlayerCard
    { pcCardId = CardId $ unAllyId (allyId a)
    , pcCardDef = allyCardDef a
    , pcOwner = Just (allyController a)
    , pcController = Just (allyController a)
    }

instance Entity Ally where
  type EntityId Ally = AllyId
  type EntityAttrs Ally = AllyAttrs
  toId = toId . toAttrs
  toAttrs (Ally a) = toAttrs a

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
