module Marvel.Treachery.Treacheries.Earthquake (
  earthquake,
  Earthquake (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

earthquake :: TreacheryCard Earthquake
earthquake = treachery Earthquake Cards.earthquake

newtype Earthquake = Earthquake (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1 (the engine places it as the card is revealed). When Revealed: discard 2
-- cards from your hand and exhaust your identity. [star] Boost: choose to either spend
-- [physical] [physical] resources or exhaust your identity; neither option is offered
-- when it cannot be resolved (RR Choose (Option)).
instance RunMessage Earthquake where
  runMessage msg t@(Earthquake attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardFromHand you 2
      exhaust you
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      let cost = MultiResourceCost [Just Physical, Just Physical]
      canPay <- canAffordCost you (toSource attrs) cost
      canExhaust <- identityMatches UnexhaustedIdentity you
      when (canPay || canExhaust) $ chooseOrRunOne you $
        [labeled "$label.spendTwoPhysicalResources" (payForTreachery you cost) | canPay]
        <> [labeled "$label.exhaustYourIdentity" (exhaust you) | canExhaust]
      pure t
    _ -> Earthquake <$> runMessage msg attrs
