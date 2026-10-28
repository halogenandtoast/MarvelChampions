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
  | -- | a wild resource spent, as the type its payer declared it (RR Wild Resource):
    -- wild until they say otherwise
    WildPayment Resource
  | ResourcePaymentFromCard ExtendedCardMatcher
  | DiscardHandCardPayment Card
  | -- | the top card of the encounter deck, discarded to pay
    DiscardedEncounterCardPayment Card
  | DamagePayment Natural
  | HealPayment Natural
  | DamageThisPayment Natural
  | ExhaustPayment
  | DiscardPayment Target
  | UsePayment
  | -- | an ally exhausted to pay
    ExhaustedAllyPayment AllyId
  | -- | a character exhausted to pay
    ExhaustedCharacterPayment CharacterId
  | -- | X, for a cost of "spend X resources": what was spent beyond the rest of the cost
    XPayment Natural
  | -- | the target chosen as the card was played, before its cost was paid
    TargetPayment Target
  | -- | so many counters discarded from the card to pay
    CountersPayment Natural
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

-- | The types the wild resources paid were declared as, in the order paid.
paymentWilds :: Payment -> [Resource]
paymentWilds p = [r | WildPayment r <- payments p]

-- | The nth wild resource paid, declared as this type.
declareWild :: Natural -> Resource -> Payment -> Payment
declareWild n r = mconcat . go n . payments
 where
  go _ [] = []
  go 0 (WildPayment _ : ps) = WildPayment r : ps
  go k (p@(WildPayment _) : ps) = p : go (k - 1) ps
  go k (p : ps) = p : go k ps

-- | The target chosen as the card was played.
paymentTarget :: Payment -> Maybe Target
paymentTarget p = headMay [t | TargetPayment t <- payments p]

-- | X, for a cost of "spend X resources".
paymentX :: Payment -> Natural
paymentX p = sum [x | XPayment x <- payments p]

-- | The cards discarded from hand to pay.
paymentDiscards :: Payment -> [Card]
paymentDiscards p = [c | DiscardHandCardPayment c <- payments p]

-- | The encounter cards discarded from the top of the deck to pay.
paymentEncounterDiscards :: Payment -> [Card]
paymentEncounterDiscards p = [c | DiscardedEncounterCardPayment c <- payments p]

-- | The allies exhausted to pay.
paymentExhaustedAllies :: Payment -> [AllyId]
paymentExhaustedAllies p = [a | ExhaustedAllyPayment a <- payments p]

-- | The characters exhausted to pay.
paymentExhaustedCharacters :: Payment -> [CharacterId]
paymentExhaustedCharacters p = [c | ExhaustedCharacterPayment c <- payments p]
