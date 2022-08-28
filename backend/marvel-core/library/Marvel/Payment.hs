module Marvel.Payment
  ( module Marvel.Payment
  , module Marvel.Payment.Types
  ) where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Payment.Types
import Marvel.Projection
import Marvel.Query
import Marvel.Queue
import Marvel.Resource

paymentResources
  :: ( Projection m PlayerIdentity
     , HasQueue m
     , HasGame m
     , MonadRandom m
     , MonadThrow m
     )
  => Payment
  -> m [Resource]
paymentResources NoPayment = pure []
paymentResources (ResourcePayment r) = pure [r]
paymentResources (ResourcePaymentFromCard matcher) = do
  cards <- selectList matcher
  case cards of
    [] -> pure []
    [x] -> pure $ printedResources $ getCardDef x
    _ -> error "target matches too many cards"
paymentResources (Payments ps) = concatMapM paymentResources ps
