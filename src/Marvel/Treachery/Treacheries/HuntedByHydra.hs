module Marvel.Treachery.Treacheries.HuntedByHydra (
  huntedByHydra,
  HuntedByHydra (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

huntedByHydra :: TreacheryCard HuntedByHydra
huntedByHydra = treachery HuntedByHydra Cards.huntedByHydra

newtype HuntedByHydra = HuntedByHydra (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Incite 1 (the engine places it as the card is revealed). "When Revealed: Each player
-- in hero form takes 1 damage and discards 1 card at random from their hand."
instance RunMessage HuntedByHydra where
  runMessage msg t@(HuntedByHydra attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      heroes <- filterM (identityMatches HeroIdentity) =<< getPlayers
      for_ heroes \p -> do
        damageIdentity p attrs (toDamage 1 FromAbility)
        discardFrom p RandomFromHand 1
      pure t
    _ -> HuntedByHydra <$> runMessage msg attrs
