module Marvel.Obligation.Obligations.SilkSenseOverloadSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W

silkSenseOverloadSilk :: ObligationCard SilkSenseOverloadSilk
silkSenseOverloadSilk = obligation SilkSenseOverloadSilk Cards.silkSenseOverloadSilk
newtype SilkSenseOverloadSilk = SilkSenseOverloadSilk (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance HasAbilities SilkSenseOverloadSilk where
  getAbilities u@(SilkSenseOverloadSilk attrs) = case obligationOwner attrs of
    Nothing -> []
    Just you -> [limitedWindowAbility u 1 (W.CardWouldBeTuckedUnderIdentity $ IdentityWithId you) ForcedInterrupt NoCriteria NoCost (runAbility u 1)]
instance RunMessage SilkSenseOverloadSilk where
  runMessage msg u@(SilkSenseOverloadSilk attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure u
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.WouldTuckEncounterCard who card <- windows, who == you] $ \card -> do
        insteadOf (\case CardSpecific (IdentityRef who) "tuckEncounterCard" _ -> who == you; _ -> False) (cardSpecific attrs "holdEncounterCard" card >> cardSpecific attrs "checkCount" you)
      pure u
    CardSpecific (isTarget attrs -> True) "checkCount" (fromJSON -> Success (you :: IdentityId)) -> do
      let n = length $ obligationHeldEncounterCards attrs
      if n >= 3 then removeFromGame attrs else when (n == 2) $ chooseOne you [labeled "$label.done" (pure ()), targeting attrs (removeFromPlay attrs)]
      pure u
    _ -> SilkSenseOverloadSilk <$> runMessage msg attrs
