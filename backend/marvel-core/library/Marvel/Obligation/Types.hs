module Marvel.Obligation.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Obligation = forall a . IsObligation a => Obligation a

instance Show Obligation where
  show (Obligation a) = show a

instance Eq Obligation where
  Obligation (a :: a) == Obligation (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Obligation where
  toJSON (Obligation a) = toJSON a

data SomeObligationCard = forall a . IsObligation a => SomeObligationCard
  (ObligationCard a)

liftObligationCard
  :: (forall a . ObligationCard a -> b) -> SomeObligationCard -> b
liftObligationCard f (SomeObligationCard a) = f a

someObligationCardCode :: SomeObligationCard -> CardCode
someObligationCardCode = liftObligationCard cbCardCode

instance Entity Obligation where
  type EntityId Obligation = ObligationId
  type EntityAttrs Obligation = ObligationAttrs
  toId = toId . toAttrs
  toAttrs (Obligation a) = toAttrs a

instance RunMessage Obligation where
  runMessage msg (Obligation a) = Obligation <$> runMessage msg a

instance IsSource Obligation where
  toSource = ObligationSource . toId

instance IsTarget Obligation where
  toTarget = ObligationTarget . toId

instance IsCard Obligation where
  toCard = toCard . toAttrs

instance HasCardDef Obligation where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a, Entity a, EntityAttrs a ~ ObligationAttrs, EntityId a ~ ObligationId) => IsObligation a

type ObligationCard a = CardBuilder ObligationId a

data ObligationAttrs = ObligationAttrs
  { obligationId :: ObligationId
  , obligationCardDef :: CardDef
  , obligationSurge :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

surgeL :: Lens' ObligationAttrs Bool
surgeL = lens obligationSurge $ \m x -> m { obligationSurge = x }

instance HasCardCode ObligationAttrs where
  toCardCode = toCardCode . obligationCardDef

obligation :: (ObligationAttrs -> a) -> CardDef -> CardBuilder ObligationId a
obligation f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ ObligationAttrs
    { obligationId = mid
    , obligationCardDef = cardDef
    , obligationSurge = False
    }
  }

instance Entity ObligationAttrs where
  type EntityId ObligationAttrs = ObligationId
  type EntityAttrs ObligationAttrs = ObligationAttrs
  toId = obligationId
  toAttrs = id

instance IsSource ObligationAttrs where
  toSource = ObligationSource . toId

instance IsTarget ObligationAttrs where
  toTarget = ObligationTarget . toId

instance IsCard ObligationAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unObligationId $ toId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef ObligationAttrs where
  getCardDef = obligationCardDef

instance RunMessage ObligationAttrs where
  runMessage msg attrs = case msg of
    ObligationMessage tid msg' | tid == toId attrs -> case msg' of
      ResolvedObligation identityId -> do
        pushAll
          $ RemoveFromPlay (toTarget attrs)
          : [ Surge identityId | obligationSurge attrs ]
        pure attrs
      _ -> pure attrs
    GainSurge target | isTarget attrs target -> pure $ attrs & surgeL .~ True
    _ -> pure attrs
