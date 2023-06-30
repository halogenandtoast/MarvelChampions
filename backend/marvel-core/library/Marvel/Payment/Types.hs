module Marvel.Payment.Types where

import Marvel.Prelude

import Marvel.Card.Types
import Marvel.Matchers.Types
import Marvel.Ref
import Marvel.Resource.Types

data Payment
  = Payments [Payment]
  | ResourcePayment Resource
  | ResourcePaymentFromCard ExtendedCardMatcher
  | DiscardHandCardPayment Card
  | DamagePayment Natural
  | HealPayment Natural
  | DamageThisPayment Natural
  | ExhaustPayment
  | DiscardPayment Target
  | UsePayment
  | NoPayment
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
