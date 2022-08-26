module Marvel.Payment where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Card.Def
import Marvel.Resource
import Marvel.Query
import Marvel.Matchers

data Payment = Payments [Payment] | ResourcePayment Resource | ResourcePaymentFromCard ExtendedCardMatcher | NoPayment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

paymentResources :: MonadGame env m => Payment -> m [Resource]
paymentResources NoPayment = pure []
paymentResources (ResourcePayment r) = pure [r]
paymentResources (ResourcePaymentFromCard matcher) = do
  cards <- selectList matcher
  case cards of
    [] -> pure []
    [x] -> pure $ printedResources $ getCardDef x
    _ -> error "target matches too many cards"
paymentResources (Payments ps) = concatMapM paymentResources ps

instance Semigroup Payment where
  NoPayment <> x = x
  x <> NoPayment = x
  Payments xs <> Payments ys = Payments $ xs <> ys
  x <> Payments ys = Payments $ x : ys
  Payments xs <> y = Payments $ xs <> [y]
  x <> y = Payments [x, y]

instance Monoid Payment where
  mempty = NoPayment


