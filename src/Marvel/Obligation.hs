{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Obligation where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Obligation.Obligations
import Marvel.Obligation.Types

instance FromJSON Obligation where
  parseJSON = withObject "Obligation" $ \o -> do
    cardDef <- o .: "obligationCardDef"
    withObligationCardCode (cdCardCode cardDef)
      $ \(_ :: ObligationCard a) -> Obligation <$> parseJSON @a (Object o)

withObligationCardCode
  :: CardCode -> (forall a . IsObligation a => ObligationCard a -> r) -> r
withObligationCardCode cCode f = case lookup cCode allObligations of
  Nothing -> error "invalid obligation"
  Just (SomeObligationCard a) -> f a

allObligations :: HashMap CardCode SomeObligationCard
allObligations = fromList $ map
  (toFst someObligationCardCode)
  [ SomeObligationCard affairsOfState
  , SomeObligationCard legalWork
  , SomeObligationCard evictionNotice
  , SomeObligationCard businessProblems
  , SomeObligationCard familyEmergency
  ]

lookupObligation :: CardCode -> ObligationId -> Obligation
lookupObligation cardCode = case lookup cardCode allObligations of
  Just (SomeObligationCard a) -> Obligation <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for obligation " <> show cardCode
