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
paymentResources (ResourcePayment r) = pure [r]
paymentResources (ResourcePaymentFromCard matcher) = do
  cards <- selectList matcher
  case cards of
    [] -> pure []
    [x] -> pure $ printedResources $ getCardDef x
    _ -> error "target matches too many cards"
paymentResources (Payments ps) = concatMapM paymentResources ps
paymentResources _ = pure []

-- | Whether a resource of this type, or a wild one, went to the payment: "if you paid
-- for this card using a [physical] resource".
paidWith
  :: ( Projection m PlayerIdentity
     , HasQueue m
     , HasGame m
     , MonadRandom m
     , MonadThrow m
     )
  => Resource
  -> Payment
  -> m Bool
paidWith r payment = any (`elem` [r, Wild]) <$> paymentResources payment

-- | Whether a card was paid for using only this type of resource: "if you paid for this
-- card using only [physical] resources". A wild resource is spent as that type; nothing
-- paid, for a cost reduced to 0, is not paying with it.
paidUsingOnly
  :: ( Projection m PlayerIdentity
     , HasQueue m
     , HasGame m
     , MonadRandom m
     , MonadThrow m
     )
  => Resource
  -> Payment
  -> m Bool
paidUsingOnly r payment = do
  resources <- paymentResources payment
  pure $ not (null resources) && all (`elem` [r, Wild]) resources
