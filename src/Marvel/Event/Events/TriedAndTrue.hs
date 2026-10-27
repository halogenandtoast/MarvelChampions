module Marvel.Event.Events.TriedAndTrue (
  triedAndTrue,
  TriedAndTrue (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getPlayers)
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

triedAndTrue :: EventCard TriedAndTrue
triedAndTrue = event TriedAndTrue Cards.triedAndTrue

newtype TriedAndTrue = TriedAndTrue (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Choose a player with a card in their discard pile (one with none cannot be
-- affected, RR Target); they may add up to 3 cards from it to their hand. Then it is
-- removed from the game, not discarded.
instance RunMessage TriedAndTrue where
  runMessage msg e@(TriedAndTrue attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        withDiscard <- filterM (fmap notNull . projectMap PlayerIdentityDiscard unDiscard) =<< getPlayers
        chooseOne you [targeting (IdentityRef p) (chooseFromDiscard p attrs AnyCards 0 3) | p <- withDiscard]
        pure e
      ResolvedEvent -> do
        removeFromGame (CardIdRef . CardId . unEventId $ eventId attrs)
        pure e
      _ -> TriedAndTrue <$> runMessage msg attrs
    WithChosen (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      for_ cards \c -> for_ (pcOwner c) \owner -> addToHand owner c
      pure e
    _ -> TriedAndTrue <$> runMessage msg attrs
