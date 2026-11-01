module Marvel.Obligation.Obligations.Nc48026 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window ()
import Marvel.Obligation.Types
import Marvel.Obligation.Cards qualified as Cards

nc48026 :: ObligationCard Nc48026
nc48026 = obligation Nc48026 Cards.nc48026

newtype Nc48026 = Nc48026 (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48026 where
  runMessage msg a@(Nc48026 attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [labeled "$label.doNotFlipToAlterEgo" (pure ()), labeled "$label.changeToB" (changeToAlterEgo you)]
      resolveObligation oid you
      pure a
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      removable <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOne you $ [labeled "$label.crisisOfFaithExhaust" (exhaust you >> removeFromGame attrs) | removable]
        <> [labeled "$label.crisisOfFaithDiscard" do
          cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches $
            CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Defense])
          for_ cards discardCard
          removeFromPlay attrs]
      pure a
    _ -> Nc48026 <$> runMessage msg attrs
