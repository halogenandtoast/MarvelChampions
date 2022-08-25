module Marvel.Payment where

import Marvel.Prelude

import Marvel.Matchers
import Marvel.Resource

data Payment = Payments [Payment] | ResourcePayment Resource | ResourcePaymentFromCard ExtendedCardMatcher | NoPayment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Payment where
  NoPayment <> x = x
  x <> NoPayment = x
  Payments xs <> Payments ys = Payments $ xs <> ys
  x <> Payments ys = Payments $ x : ys
  Payments xs <> y = Payments $ xs <> [y]
  x <> y = Payments [x, y]

instance Monoid Payment where
  mempty = NoPayment

