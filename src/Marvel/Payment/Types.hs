module Marvel.Payment.Types where

import Marvel.Prelude

import Marvel.Card.Types
import Marvel.Id
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
  | -- | an ally exhausted to pay
    ExhaustedAllyPayment AllyId
  | -- | X, for a cost of "spend X resources": what was spent beyond the rest of the cost
    XPayment Natural
  | -- | the target chosen as the card was played, before its cost was paid
    TargetPayment Target
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

-- | Each payment made, one by one.
payments :: Payment -> [Payment]
payments = \case
  Payments ps -> concatMap payments ps
  NoPayment -> []
  p -> [p]

-- | The target chosen as the card was played.
paymentTarget :: Payment -> Maybe Target
paymentTarget p = headMay [t | TargetPayment t <- payments p]

-- | X, for a cost of "spend X resources".
paymentX :: Payment -> Natural
paymentX p = sum [x | XPayment x <- payments p]

-- | The cards discarded from hand to pay.
paymentDiscards :: Payment -> [Card]
paymentDiscards p = [c | DiscardHandCardPayment c <- payments p]

-- | The allies exhausted to pay.
paymentExhaustedAllies :: Payment -> [AllyId]
paymentExhaustedAllies p = [a | ExhaustedAllyPayment a <- payments p]
