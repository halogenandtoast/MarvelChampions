module Marvel.Payment.Types where

import Marvel.Prelude

import Marvel.Cost.Types (Cost)
import Marvel.Card.Types
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Ref
import Marvel.Resource.Types

data Payment
  = Payments [Payment]
  | -- | an ally-held event detached for payment, restored on abort
    ReservedAllyCardPayment AllyId PlayerCard
  | ReservedSupportCardPayment SupportId PlayerCard
  | -- | all non-resource alliance contributions approved before any is committed
    ApprovedAlliancePayment [(IdentityId, Cost)]
  | -- | resources actually spent after surplus generation is discarded; raw receipts remain
    ActualResourcesPayment [Resource]
  | -- | Icons on a payment card while it was still in hand (Haywire).
    PrintedResourcesPayment [Resource]
  | ResourcePayment Resource
  | -- | a wild resource spent, as the type its payer declared it (RR Wild Resource):
    -- wild until they say otherwise
    WildPayment Resource
  | -- | a contributor chooses the type of their own wild resource
    ContributedWildPayment IdentityId Resource
  | ResourcePaymentFromCard ExtendedCardMatcher
  | DiscardHandCardPayment Card
  | -- | the printed ally card sacrificed to pay, even after it leaves play
    DiscardedAllyPayment Card
  | -- | the top card of the encounter deck, discarded to pay
    DiscardedEncounterCardPayment Card
  | DamagePayment Natural
  | HealPayment Natural
  | DamageThisPayment Natural
  | ExhaustPayment
  | DiscardPayment Target
  | -- | the particular minion selected to enter play as a search cost
    SearchedMinionPayment MinionId
  | UsePayment
  | -- | an ally exhausted to pay
    ExhaustedAllyPayment AllyId
  | ExhaustedCardPayment Target
  | -- | a character exhausted to pay
    ExhaustedCharacterPayment CharacterId
  | -- | a character chosen as a cost, without being exhausted
    ChosenCharacterPayment CharacterId
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
paymentWilds = map snd . paymentWildOwners

-- | Wild resources paired with their contributor (Nothing is the main payer).
paymentWildOwners :: Payment -> [(Maybe IdentityId, Resource)]
paymentWildOwners p = mapMaybe owner (payments p)
 where
  owner (WildPayment r) = Just (Nothing, r)
  owner (ContributedWildPayment who r) = Just (Just who, r)
  owner _ = Nothing

-- | Mark the newly generated wild resources as their contributor's to declare.
attributeWilds :: Natural -> IdentityId -> Payment -> Payment
attributeWilds start who = mconcat . go start . payments
 where
  go _ [] = []
  go 0 (WildPayment r : ps) = ContributedWildPayment who r : go 0 ps
  go n (p@(WildPayment _) : ps) = p : go (n - 1) ps
  go n (p@(ContributedWildPayment _ _) : ps) = p : go (subtractNatural 1 n) ps
  go n (p : ps) = p : go n ps

-- | The nth wild resource paid, declared as this type.
declareWild :: Natural -> Resource -> Payment -> Payment
declareWild n r = mconcat . go n . payments
 where
  go _ [] = []
  go 0 (WildPayment _ : ps) = WildPayment r : ps
  go 0 (ContributedWildPayment who _ : ps) = ContributedWildPayment who r : ps
  go k (p@(WildPayment _) : ps) = p : go (k - 1) ps
  go k (p@(ContributedWildPayment _ _) : ps) = p : go (k - 1) ps
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

-- | The printed cards of the allies discarded to pay.
paymentDiscardedAllies :: Payment -> [Card]
paymentDiscardedAllies p = [c | DiscardedAllyPayment c <- payments p]

-- | Provenance of events reserved from allies while their costs are being paid.
paymentReservedAllyCards :: Payment -> [(AllyId, PlayerCard)]
paymentReservedAllyCards p = [(a, c) | ReservedAllyCardPayment a c <- payments p]

paymentApprovedAlliance :: Payment -> Maybe [(IdentityId, Cost)]
paymentApprovedAlliance p = headMay [cs | ApprovedAlliancePayment cs <- payments p]

-- | Distinct characters selected for choose-only costs, in order.
paymentChosenCharacters :: Payment -> [CharacterId]
paymentChosenCharacters p = [c | ChosenCharacterPayment c <- payments p]

-- | The final actual spend, after choosing how surplus resources are wasted.
paymentActualResources :: Payment -> Maybe [Resource]
paymentActualResources p = lastMay [rs | ActualResourcesPayment rs <- payments p]

-- | Support-held events detached while paying, restored on abort.
paymentReservedSupportCards :: Payment -> [(SupportId, PlayerCard)]
paymentReservedSupportCards p = [(s, c) | ReservedSupportCardPayment s c <- payments p]

paymentExhaustedCards :: Payment -> [Target]
paymentExhaustedCards p = [t | ExhaustedCardPayment t <- payments p]
