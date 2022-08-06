{-# LANGUAGE TemplateHaskell #-}

module Marvel.Obligation where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Obligation.Attrs
import Marvel.Obligation.Obligations
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Obligation = forall a. IsObligation a => Obligation a

instance Show Obligation where
  show (Obligation a) = show a

instance Eq Obligation where
  (Obligation (a :: a)) == (Obligation (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Obligation where
  toJSON (Obligation a) = toJSON a

instance FromJSON Obligation where
  parseJSON v = flip (withObject "Obligation") v $ \o -> do
    cardDef <- o .: "obligationCardDef"
    withObligationCardCode (cdCardCode cardDef) $ \(_ :: ObligationCard a) -> Obligation <$> parseJSON @a v

withObligationCardCode
  :: CardCode
  -> (forall a. IsObligation a => ObligationCard a -> r)
  -> r
withObligationCardCode cCode f =
  case lookup cCode allObligations of
    Nothing -> error "invalid obligation"
    Just (SomeObligationCard a) -> f a

data SomeObligationCard = forall a. IsObligation a => SomeObligationCard (ObligationCard a)

liftObligationCard :: (forall a . ObligationCard a -> b) -> SomeObligationCard -> b
liftObligationCard f (SomeObligationCard a) = f a

someObligationCardCode :: SomeObligationCard -> CardCode
someObligationCardCode = liftObligationCard cbCardCode

allObligations :: HashMap CardCode SomeObligationCard
allObligations =
  fromList $
    map
      (toFst someObligationCardCode)
      [ SomeObligationCard affairsOfState
      , SomeObligationCard legalWork
      , SomeObligationCard evictionNotice
      , SomeObligationCard businessProblems
      -- , SomeObligationCard familyEmergency
      ]

lookupObligation :: CardCode -> ObligationId -> Obligation
lookupObligation cardCode = case lookup cardCode allObligations of
  Just (SomeObligationCard a) -> Obligation <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for obligation " <> show cardCode

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
