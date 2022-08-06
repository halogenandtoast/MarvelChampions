module Marvel.Ally where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Ally.Allies
import Marvel.Ally.Attrs
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Text.Show qualified

data Ally = forall a. IsAlly a => Ally a

instance Show Ally where
  show (Ally a) = show a

instance ToJSON Ally where
  toJSON (Ally a) = toJSON a

instance Eq Ally where
  (Ally (a :: a)) == (Ally (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Ally where
  parseJSON v = flip (withObject "Ally") v $ \o -> do
    cardDef <- o .: "allyCardDef"
    withAllyCardCode (cdCardCode cardDef) $ \(_ :: AllyCard a) -> Ally <$> parseJSON @a v

withAllyCardCode
  :: CardCode
  -> (forall a. IsAlly a => AllyCard a -> r)
  -> r
withAllyCardCode cCode f =
  case lookup cCode allAllies of
    Nothing -> error "invalid ally"
    Just (SomeAllyCard a) -> f a

data SomeAllyCard = forall a. IsAlly a => SomeAllyCard (AllyCard a)

liftAllyCard :: (forall a . AllyCard a -> b) -> SomeAllyCard -> b
liftAllyCard f (SomeAllyCard a) = f a

someAllyCardCode :: SomeAllyCard -> CardCode
someAllyCardCode = liftAllyCard cbCardCode

allAllies :: HashMap CardCode SomeAllyCard
allAllies =
  fromList $
    map
      (toFst someAllyCardCode)
      [ SomeAllyCard blackCatFeliciaHardy
      , SomeAllyCard spiderWomanJessicaDrew
      , SomeAllyCard hellcatPatsyWalker
      , SomeAllyCard warMachineJamesRhodes
      , SomeAllyCard shuri
      , SomeAllyCard hulkBruceBanner
      , SomeAllyCard tigraGreerGrantNelson
      , SomeAllyCard daredevilMattMurdock
      , SomeAllyCard jessicaJones
      , SomeAllyCard hawkeyeClintBarton
      , SomeAllyCard mariaHill
      , SomeAllyCard vision
      , SomeAllyCard blackWidowNatashaRomanoff
      , SomeAllyCard lukeCage
      , SomeAllyCard mockingbirdBobbiMorse
      , SomeAllyCard nickFury
      ]

lookupAlly :: CardCode -> IdentityId -> AllyId -> Ally
lookupAlly cardCode identityId allyId = case lookup cardCode allAllies of
  Just (SomeAllyCard a) -> Ally $ cbCardBuilder a (identityId, allyId)
  Nothing -> error $ "Invalid card code for ally " <> show cardCode

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

instance HasAbilities Ally where
  getAbilities (Ally a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
        a
        300
        Basic
        (SchemeExists ThwartableScheme)
        ExhaustCost
        (AllyThwart $ toId a)
      | unThw (allyThwart $ toAttrs a) > 0
      ]
        <> [ ability
              a
              301
              Basic
              (EnemyExists AttackableEnemy)
              ExhaustCost
              (AllyAttack $ toId a)
           ]
