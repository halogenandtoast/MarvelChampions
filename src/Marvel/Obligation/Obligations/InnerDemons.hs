module Marvel.Obligation.Obligations.InnerDemons (
  innerDemons,
  InnerDemons (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Queue

innerDemons :: ObligationCard InnerDemons
innerDemons = obligation InnerDemons Cards.innerDemons

newtype InnerDemons = InnerDemons (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Change form, then: as Bruce Banner discard 2 cards from your hand, as Hulk exhaust
-- your hero. The obligation is discarded after either.
instance RunMessage InnerDemons where
  runMessage msg o@(InnerDemons attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        pushAll
          [ IdentityMessage identityId ChooseOtherForm
          , ObligationMessage (obligationId attrs) $ ResolveObligation identityId
          ]
        pure o
      ResolveObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        push $
          IdentityMessage identityId $
            if isHero then ExhaustedIdentity else DiscardFrom FromHand 2 Nothing
        pure o
      _ -> InnerDemons <$> runMessage msg attrs
    _ -> InnerDemons <$> runMessage msg attrs
