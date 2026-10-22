module Marvel.Obligation.Obligations.InnerDemons (
  innerDemons,
  InnerDemons (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

innerDemons :: ObligationCard InnerDemons
innerDemons = obligation InnerDemons Cards.innerDemons

newtype InnerDemons = InnerDemons (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Change form, then: as Bruce Banner discard 2 cards from your hand, as Hulk exhaust
-- your hero. The obligation is discarded after either.
instance RunMessage InnerDemons where
  runMessage msg o@(InnerDemons attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        changeForm identityId
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero then exhaust identityId else discardFrom identityId FromHand 2
        pure o
      _ -> InnerDemons <$> runMessage msg attrs
    _ -> InnerDemons <$> runMessage msg attrs
