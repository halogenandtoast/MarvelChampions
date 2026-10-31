module Marvel.Event.Events.TrackByScent (
  trackByScent,
  TrackByScent (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

trackByScent :: EventCard TrackByScent
trackByScent = event TrackByScent Cards.trackByScent

newtype TrackByScent = TrackByScent (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Draw only when this thwart actually removes the last threat.
instance RunMessage TrackByScent where
  runMessage msg e@(TrackByScent attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne identityId
        [ targeting (SchemeRef s) do
            thwartScheme s attrs 3
            cardSpecific attrs "check" s
        | s <- schemes
        ]
      pure e
    CheckWindows ws -> do
      for_ [scheme | W.Window W.After (W.ThwartRemovedThreat source scheme before removed) <- ws, source == toSource attrs, before > 0, removed == before] $ \scheme ->
        insteadOf (isCheck scheme) $ cardSpecific attrs "draw" ()
      TrackByScent <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "draw" _ -> e <$ drawCards (eventController attrs) 2
    _ -> TrackByScent <$> runMessage msg attrs
   where
    isCheck scheme = \case
      CardSpecific (isTarget attrs -> True) "check" (fromJSON -> Success scheme') -> scheme == scheme'
      _ -> False
