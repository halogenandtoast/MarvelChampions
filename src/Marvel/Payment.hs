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
  :: ( HasQueue m
     , HasGame m
     , MonadThrow m
     )
  => Payment
  -> m [Resource]
paymentResources payment = case paymentActualResources payment of
  Just resources -> pure resources
  Nothing -> concatMapM generated (payments payment)
 where
  generated (PhysicalResourcesForTough ident) = do
    tough <- project PlayerIdentityTough ident
    extra <- project PlayerIdentityAdditionalTough ident
    pure $ replicate (fromIntegral $ (if tough then 1 else 0) + extra) Physical
  generated (ResourcePayment r) = pure [r]
  generated (WildPayment r) = pure [r]
  generated (ContributedWildPayment _ r) = pure [r]
  generated (ResourcePaymentFromCard matcher) = do
    cards <- selectList matcher
    case cards of
      [] -> pure []
      [x] -> pure $ printedResources $ getCardDef x
      _ -> error "target matches too many cards"
  generated _ = pure []

-- | Whether a resource of this type went to the payment: "if you paid for this card
-- using a [physical] resource". A wild resource counts as the type it was declared as,
-- and only that (RR Wild Resource).
paidWith
  :: ( HasQueue m
     , HasGame m
     , MonadThrow m
     )
  => Resource
  -> Payment
  -> m Bool
paidWith r payment = elem r <$> paymentResources payment

-- | Whether a card was paid for using only this type of resource: "if you paid for this
-- card using only [physical] resources". A wild resource counts as the type it was
-- declared as; nothing paid, for a cost reduced to 0, is not paying with it.
paidUsingOnly
  :: ( HasQueue m
     , HasGame m
     , MonadThrow m
     )
  => Resource
  -> Payment
  -> m Bool
paidUsingOnly r payment = do
  resources <- paymentResources payment
  pure $ not (null resources) && all (== r) resources
