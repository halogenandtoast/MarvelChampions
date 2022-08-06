module Marvel.AlterEgo where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Trait
import Text.Show qualified

data AlterEgo = forall a. IsAlterEgo a => AlterEgo a

instance Show AlterEgo where
  show (AlterEgo a) = show a

instance ToJSON AlterEgo where
  toJSON (AlterEgo a) = toJSON a

instance Eq AlterEgo where
  (AlterEgo (a :: a)) == (AlterEgo (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON AlterEgo where
  parseJSON v = flip (withObject "AlterEgo") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withAlterEgoCardCode cCode $ \(_ :: AlterEgoCard a) -> AlterEgo <$> parseJSON @a v

withAlterEgoCardCode
  :: CardCode
  -> (forall a. IsAlterEgo a => AlterEgoCard a -> r)
  -> r
withAlterEgoCardCode cCode f =
  case lookup cCode allAlterEgos of
    Nothing -> error "invalid alter ego"
    Just (SomeAlterEgoCard a) -> f a

data SomeAlterEgoCard = forall a. IsAlterEgo a => SomeAlterEgoCard (AlterEgoCard a)

liftAlterEgoCard :: (forall a . AlterEgoCard a -> b) -> SomeAlterEgoCard -> b
liftAlterEgoCard f (SomeAlterEgoCard a) = f a

someAlterEgoCardCode :: SomeAlterEgoCard -> CardCode
someAlterEgoCardCode = liftAlterEgoCard cbCardCode

allAlterEgos :: HashMap CardCode SomeAlterEgoCard
allAlterEgos =
  fromList $
    map
      (toFst someAlterEgoCardCode)
      [ SomeAlterEgoCard peterParker
      , SomeAlterEgoCard carolDanvers
      , SomeAlterEgoCard jenniferWalters
      , SomeAlterEgoCard tonyStark
      , SomeAlterEgoCard tChalla
      ]

lookupAlterEgoByCardCode :: CardCode -> IdentityId -> AlterEgo
lookupAlterEgoByCardCode cardCode = case lookup cardCode allAlterEgos of
  Nothing -> error $ "Unknown alter ego: " <> show cardCode
  Just (SomeAlterEgoCard a) -> AlterEgo <$> cbCardBuilder a

instance RunMessage AlterEgo where
  runMessage msg (AlterEgo a) = AlterEgo <$> runMessage msg a

instance HasTraits AlterEgo where
  getTraits = pure . cdTraits . getCardDef

instance HasAbilities AlterEgo where
  getAbilities (AlterEgo a) = getAbilities a <> basicAbilities
   where
    basicAbilities = [ability a 200 Basic NoCriteria ExhaustCost Recover]

instance HasStartingHP AlterEgo where
  startingHP = startingHP . toAttrs

instance HasHandSize AlterEgo where
  handSize = handSize . toAttrs

instance HasCardCode AlterEgo where
  toCardCode = toCardCode . toAttrs

instance HasCardDef AlterEgo where
  getCardDef = getCardDef . toAttrs

instance IsSource AlterEgo where
  toSource = toSource . toAttrs

instance Entity AlterEgo where
  type EntityId AlterEgo = IdentityId
  type EntityAttrs AlterEgo = AlterEgoAttrs
  toId = toId . toAttrs
  toAttrs (AlterEgo a) = toAttrs a

instance HasModifiersFor AlterEgo where
  getModifiersFor source target (AlterEgo a) = getModifiersFor source target a
