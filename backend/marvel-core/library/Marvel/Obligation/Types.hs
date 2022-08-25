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
  type Id Obligation = ObligationId
  data Attrs Obligation = ObligationAttrs
    { obligationId :: ObligationId
    , obligationCardDef :: CardDef
    , obligationSurge :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Obligation :: Type -> Type where
    ObligationId :: Field Obligation ObligationId
    ObligationCardDef :: Field Obligation CardDef
    ObligationSurge :: Field Obligation Bool
  toId = obligationId . toAttrs
  toAttrs (Obligation a) = toObligationAttrs a

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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a)  => IsObligation a where
  toObligationAttrs :: a -> Attrs Obligation
  default toObligationAttrs :: Coercible a (Attrs Obligation) => a -> Attrs Obligation
  toObligationAttrs = coerce

type ObligationCard a = CardBuilder ObligationId a

surgeL :: Lens' (Attrs Obligation) Bool
surgeL = lens obligationSurge $ \m x -> m { obligationSurge = x }

instance HasCardCode (Attrs Obligation) where
  toCardCode = toCardCode . obligationCardDef

obligation :: (Attrs Obligation -> a) -> CardDef -> CardBuilder ObligationId a
obligation f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ ObligationAttrs
    { obligationId = mid
    , obligationCardDef = cardDef
    , obligationSurge = False
    }
  }

instance IsSource (Attrs Obligation) where
  toSource = ObligationSource . obligationId

instance IsTarget (Attrs Obligation) where
  toTarget = ObligationTarget . obligationId

instance IsCard (Attrs Obligation) where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unObligationId $ obligationId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef (Attrs Obligation) where
  getCardDef = obligationCardDef

instance RunMessage (Attrs Obligation) where
  runMessage msg attrs = case msg of
    ObligationMessage ident msg' | ident == obligationId attrs -> case msg' of
      ResolvedObligation identityId -> do
        pushAll
          $ RemoveFromPlay (toTarget attrs)
          : [ Surge identityId | obligationSurge attrs ]
        pure attrs
      _ -> pure attrs
    GainSurge target | isTarget attrs target -> pure $ attrs & surgeL .~ True
    _ -> pure attrs
