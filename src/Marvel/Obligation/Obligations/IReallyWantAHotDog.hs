module Marvel.Obligation.Obligations.IReallyWantAHotDog where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ref

iReallyWantAHotDog :: ObligationCard IReallyWantAHotDog
iReallyWantAHotDog = obligation IReallyWantAHotDog Cards.iReallyWantAHotDog

newtype IReallyWantAHotDog = IReallyWantAHotDog (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage IReallyWantAHotDog where
  runMessage msg o@(IReallyWantAHotDog attrs) = runQueueT $ case msg of
    ObligationMessage ident (RevealObligation you) | ident == obligationId attrs -> do
      whenM (identityMatches HeroIdentity you) $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation (obligationId attrs) you
      pure o
    ObligationMessage ident (ResolveObligation you) | ident == obligationId attrs -> do
      eligible <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      counters <- getCountersOn you
      chooseOrRunOne you $
        [labeled "$label.exhaustPeterPorkerRemoveHotDog" (exhaust you >> removeCounters you 1 >> removeFromGame attrs) | eligible && counters > 0]
        <> [labeled "$label.stunnedDiscardHotDog" do
              already <- identityMatches StunnedIdentity you
              stunIdentity you
              when already (gainSurge attrs)]
      pure o
    _ -> IReallyWantAHotDog <$> runMessage msg attrs
