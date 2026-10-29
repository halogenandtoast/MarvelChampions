module Marvel.Ally.Types (
  module Marvel.Ally.Types,
  module X,
) where

import Marvel.Prelude

import Marvel.Card as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Modifier as X
import Marvel.Ref as X
import Marvel.Stats as X

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Typeable
import Data.HashSet qualified as HashSet
import Marvel.Ability.Types
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Game.Source
import Marvel.Id hiding (AllyId)
import Marvel.Id qualified as Id
import Marvel.Message hiding (AllyStunned)
import Marvel.Question hiding (AllyAttack, AllyThwart)
import Marvel.Trait

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, HasAbilities a, RunMessage a) => IsAlly a where
  toAllyAttrs :: a -> Attrs Ally
  default toAllyAttrs :: (Coercible a (Attrs Ally)) => a -> Attrs Ally
  toAllyAttrs = coerce

type AllyCard a = CardBuilder (IdentityId, Id.AllyId) a

data Ally = forall a. (IsAlly a) => Ally a

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a

instance Eq Ally where
  Ally (a :: a) == Ally (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAllyCard = forall a. (IsAlly a) => SomeAllyCard (AllyCard a)

liftAllyCard :: (forall a. AllyCard a -> b) -> SomeAllyCard -> b
liftAllyCard f (SomeAllyCard a) = f a

someAllyCardCode :: SomeAllyCard -> CardCode
someAllyCardCode = liftAllyCard toCardCode

upgradesL :: Lens' (Attrs Ally) (HashSet UpgradeId)
upgradesL = lens allyUpgrades $ \m x -> m {allyUpgrades = x}

countersL :: Lens' (Attrs Ally) Natural
countersL = lens allyCounters $ \m x -> m {allyCounters = x}

heldCardsL :: Lens' (Attrs Ally) [PlayerCard]
heldCardsL = lens allyHeldCards $ \m x -> m {allyHeldCards = x}

damageL :: Lens' (Attrs Ally) Natural
damageL = lens allyDamage $ \m x -> m {allyDamage = x}

toughL :: Lens' (Attrs Ally) Bool
toughL = lens allyTough $ \m x -> m {allyTough = x}

confusedL :: Lens' (Attrs Ally) Bool
confusedL = lens allyConfused $ \m x -> m {allyConfused = x}

stunnedL :: Lens' (Attrs Ally) Bool
stunnedL = lens allyStunned $ \m x -> m {allyStunned = x}

exhaustedL :: Lens' (Attrs Ally) Bool
exhaustedL = lens allyExhausted $ \m x -> m {allyExhausted = x}

instance HasController (Attrs Ally) where
  controller = allyController

controllerMessage :: (IsAlly a) => a -> IdentityMessage -> Message
controllerMessage a = IdentityMessage (allyController $ toAllyAttrs a)

instance HasCardCode (Attrs Ally) where
  toCardCode = toCardCode . allyCardDef

instance HasCardDef (Attrs Ally) where
  getCardDef = allyCardDef

allyWith ::
  (Attrs Ally -> a) ->
  CardDef ->
  (Thw, Natural) ->
  (Atk, Natural) ->
  HP Natural ->
  (Attrs Ally -> Attrs Ally) ->
  CardBuilder (IdentityId, Id.AllyId) a
allyWith f cardDef thwPair atkPair hp g =
  ally (f . g) cardDef thwPair atkPair hp

ally ::
  (Attrs Ally -> a) ->
  CardDef ->
  (Thw, Natural) ->
  (Atk, Natural) ->
  HP Natural ->
  CardBuilder (IdentityId, Id.AllyId) a
ally f cardDef (thw, thwConsequentialDamage) (atk, atkConsequentialDamage) hp =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (ident, aid) ->
        f $
          AllyAttrs
            { allyId = aid
            , allyCardDef = cardDef
            , allyDamage = 0
            , allyAttack = atk
            , allyAttackConsequentialDamage = atkConsequentialDamage
            , allyThwart = thw
            , allyThwartConsequentialDamage = thwConsequentialDamage
            , allyController = ident
            , allyOwner = ident
            , allyHitPoints = hp
            , allyExhausted = False
            , allyStunned = False
            , allyConfused = False
            , allyTough = False
            , allyCounters = 0
            , allyUpgrades = mempty
            , allyAdditionalAttackCost = NoCost
            , allyPaidWith = NoPayment
            , allyHeldCards = []
            }
    }

instance IsRef (Attrs Ally) where
  toRef = AllyRef . allyId

getModifiedAttack :: (HasGame m) => Attrs Ally -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ allyAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedHitPoints :: (HasGame m) => Attrs Ally -> m Natural
getModifiedHitPoints attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unHp $ allyHitPoints attrs) modifiers
 where
  applyModifier (HitPointModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

-- | The consequential damage it takes after it attacks.
getModifiedAttackConsequentialDamage :: (HasGame m) => Attrs Ally -> m Natural
getModifiedAttackConsequentialDamage attrs = do
  modifiers <- getModifiers attrs
  pure $
    if NoConsequentialDamage `elem` modifiers
      then 0
      else subtractNatural (sum [n | ConsequentialDamageReduction n <- modifiers]) $
        allyAttackConsequentialDamage attrs + sum [n | AttackConsequentialDamageModifier n <- modifiers]

getModifiedThwartConsequentialDamage :: (HasGame m) => Attrs Ally -> m Natural
getModifiedThwartConsequentialDamage attrs = do
  modifiers <- getModifiers attrs
  pure $ if NoConsequentialDamage `elem` modifiers then 0 else
    subtractNatural (sum [n | ConsequentialDamageReduction n <- modifiers]) (allyThwartConsequentialDamage attrs)

getModifiedThwart :: (HasGame m) => Attrs Ally -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ subtractNatural (sum [n | ThwartPenalty n <- modifiers]) $
    unThw (allyThwart attrs) + sum [n | ThwartModifier n <- modifiers]

instance IsCard (Attrs Ally) where
  toCard a =
    PlayerCard $
      MkPlayerCard
        { pcCardId = CardId $ unAllyId (allyId a)
        , pcCardDef = allyCardDef a
        , pcOwner = Just (allyOwner a)
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
    , allyOwner :: IdentityId
    -- ^ whose deck it came from, and whose discard pile it goes to
    , allyExhausted :: Bool
    , allyCounters :: Natural
    , allyUpgrades :: HashSet UpgradeId
    , allyStunned :: Bool
    , allyConfused :: Bool
    , allyTough :: Bool
    , allyAdditionalAttackCost :: Cost
    -- ^ paid on top of exhausting it, for its basic attack
    , allyHeldCards :: [PlayerCard]
    -- ^ facedown events playable from here while this ally is not blank
    , allyPaidWith :: Payment
    -- ^ what was paid to play it, for "if you paid for this card using …"
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
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
    AllyHeldCards :: Field Ally [PlayerCard]
  toId = allyId . toAttrs
  toAttrs (Ally a) = toAllyAttrs a
  field fld a =
    let AllyAttrs {..} = toAttrs a
     in case fld of
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
          AllyHeldCards -> allyHeldCards

instance RunMessage Ally where
  runMessage msg (Ally a) = Ally <$> runMessage msg a

instance Exhaustable Ally where
  isExhausted = allyExhausted . toAttrs

instance IsCard Ally where
  toCard = toCard . toAttrs

instance IsRef Ally where
  toRef = AllyRef . toId

getAllyController :: Ally -> IdentityId
getAllyController = allyController . toAttrs

getAllyUses :: Ally -> Natural
getAllyUses = allyCounters . toAttrs

getAllyDamage :: Ally -> Natural
getAllyDamage = allyDamage . toAttrs

instance HasCardDef Ally where
  getCardDef = getCardDef . toAttrs

instance HasTraits Ally where
  getTraits a = (cdTraits (getCardDef a) <>) <$> getGainedTraits a

instance HasModifiersFor Ally where
  getModifiersFor source target (Ally a) = getModifiersFor source target a

-- Saved games from before allies could hold events have no attached cards.
instance FromJSON (Attrs Ally) where
  parseJSON = withObject "AllyAttrs" $ \o ->
    genericParseJSON defaultOptions (Object $ if KeyMap.member "allyHeldCards" o then o else KeyMap.insert "allyHeldCards" (toJSON ([] :: [PlayerCard])) o)
