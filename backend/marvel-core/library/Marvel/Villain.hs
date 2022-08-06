module Marvel.Villain where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.Attack
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Villain.Attrs
import Marvel.Villain.Villains.Klaw
import Marvel.Villain.Villains.Rhino
import Text.Show qualified

data Villain = forall a. IsVillain a => Villain a

instance Show Villain where
  show (Villain a) = show a

instance ToJSON Villain where
  toJSON (Villain a) = toJSON a

instance Eq Villain where
  (Villain (a :: a)) == (Villain (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Villain where
  parseJSON v = flip (withObject "Villain") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withVillainCardCode cCode $ \(_ :: VillainCard a) -> Villain <$> parseJSON @a v

withVillainCardCode
  :: CardCode
  -> (forall a. IsVillain a => VillainCard a -> r)
  -> r
withVillainCardCode cCode f =
  case lookup cCode allVillains of
    Nothing -> error "invalid villain"
    Just (SomeVillainCard a) -> f a

instance Entity Villain where
  type EntityId Villain = VillainId
  type EntityAttrs Villain = VillainAttrs
  toId = toId . toAttrs
  toAttrs (Villain a) = toAttrs a

lookupVillain :: CardCode -> VillainId -> Maybe Villain
lookupVillain cardCode villainId = case lookup cardCode allVillains of
  Just (SomeVillainCard a) -> Just . Villain $ cbCardBuilder a villainId
  Nothing -> Nothing

data SomeVillainCard = forall a. IsVillain a => SomeVillainCard (VillainCard a)

liftVillainCard :: (forall a . VillainCard a -> b) -> SomeVillainCard -> b
liftVillainCard f (SomeVillainCard a) = f a

someVillainCardCode :: SomeVillainCard -> CardCode
someVillainCardCode = liftVillainCard cbCardCode

allVillains :: HashMap CardCode SomeVillainCard
allVillains =
  fromList $
    map (toFst someVillainCardCode)
    [ SomeVillainCard rhino
    , SomeVillainCard klaw
    ]

villainDamage :: Villain -> Natural
villainDamage v =
  fromIntegral . max 0 $
    unHp (villainMaxHp attrs)
      - unHp
        (villainHp attrs)
 where
  attrs = toAttrs v

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
