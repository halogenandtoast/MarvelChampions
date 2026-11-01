module Marvel.Obligation.Obligations.FearTheFuture where

import Marvel.Prelude
import Marvel.Card
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

fearTheFuture :: ObligationCard FearTheFuture
fearTheFuture = obligation FearTheFuture Cards.fearTheFuture

newtype FearTheFuture = FearTheFuture (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage FearTheFuture where
  runMessage msg a@(FearTheFuture attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      whenM (identityMatches HeroIdentity you) $ chooseOne you
        [labeled "$label.doNotFlipToAlterEgo" (pure ()), labeled "$label.changeToB" (changeToAlterEgo you)]
      resolveObligation oid you
      pure a
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOrRunOne you $
        [labeled "$label.exhaustLucasBishopRemoveFearTheFuture" (exhaust you >> removeFromGame attrs) | canExhaust]
        <> [labeled "$label.discardResourcesFearTheFuture" $ do
          cards <- filter (cardMatch $ CardWithType ResourceType) <$> projectMap PlayerIdentityHand unHand you
          for_ cards discardCard
          discardCard attrs
          when (null cards) $ surge you]
      pure a
    _ -> FearTheFuture <$> runMessage msg attrs
