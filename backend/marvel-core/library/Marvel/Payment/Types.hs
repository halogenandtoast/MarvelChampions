module Marvel.Payment.Types where

import Marvel.Prelude

import Marvel.Resource.Types
import Marvel.Matchers.Types

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
