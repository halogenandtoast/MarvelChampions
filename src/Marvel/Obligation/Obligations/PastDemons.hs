module Marvel.Obligation.Obligations.PastDemons where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

pastDemons :: ObligationCard PastDemons
pastDemons = obligation PastDemons Cards.pastDemons

newtype PastDemons = PastDemons (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PastDemons where
  runMessage msg e@(PastDemons attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [labeled "$label.doNotFlipToAlterEgo" (pure ()), labeled "$label.changeToB" (changeToAlterEgo you)]
      resolveObligation oid you
      pure $ PastDemons attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      ae <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOne you $
        [labeled "$label.exhaustLoganRemovePastDemons" (exhaust you >> removeFromGame attrs) | ae]
        <> [labeled "$label.pastDemonsStunConfuse" (giveStatus you StunnedStatus >> giveStatus you ConfusedStatus >> removeFromPlay attrs)]
      pure e
    _ -> PastDemons <$> runMessage msg attrs
