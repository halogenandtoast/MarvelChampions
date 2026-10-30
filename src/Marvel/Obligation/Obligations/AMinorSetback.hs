module Marvel.Obligation.Obligations.AMinorSetback where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Ref

aMinorSetback :: ObligationCard AMinorSetback
aMinorSetback = obligation AMinorSetback Cards.aMinorSetback

newtype AMinorSetback = AMinorSetback (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AMinorSetback where
  runMessage msg o@(AMinorSetback attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      counters <- project PlayerIdentityCounters you
      if counters > 0
        then removeCounters you 1
        else do
          dealEncounterCard you
          removeFromGame attrs
          shuffleIntoEncounterDeck [case toCard attrs of EncounterCard card -> card; _ -> error "Expected obligation encounter card"]
      pure o
    _ -> AMinorSetback <$> runMessage msg attrs
