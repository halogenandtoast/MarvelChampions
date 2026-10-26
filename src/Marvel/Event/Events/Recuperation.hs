module Marvel.Event.Events.Recuperation (
  recuperation,
  Recuperation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.AlterEgo.Runner (getModifiedRecovery)
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (PlayerIdentitySide, PlayerIdentitySides), PlayerIdentitySide (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

recuperation :: EventCard Recuperation
recuperation = event Recuperation Cards.recuperation

newtype Recuperation = Recuperation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: heal damage from your alter-ego equal to its REC; no use of REC
-- (nothing answering a recovery sees it).
instance RunMessage Recuperation where
  runMessage msg e@(Recuperation attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      case lookup side sides of
        Just (AlterEgoSide ae) -> heal you =<< getModifiedRecovery (toAttrs ae)
        _ -> pure ()
      pure e
    _ -> Recuperation <$> runMessage msg attrs
