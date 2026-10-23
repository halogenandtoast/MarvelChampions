module Marvel.Event.Events.MomentOfTriumph (
  momentOfTriumph,
  MomentOfTriumph (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

momentOfTriumph :: EventCard MomentOfTriumph
momentOfTriumph = event MomentOfTriumph Cards.momentOfTriumph

newtype MomentOfTriumph = MomentOfTriumph (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you attack and defeat an enemy, heal 1 damage from your hero for
-- each point of excess damage dealt to that enemy by that attack.
instance RunMessage MomentOfTriumph where
  runMessage msg e@(MomentOfTriumph attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just (W.DefeatedMinion _ _ _ excess))) | ident == eventId attrs -> do
      when (excess > 0) $ heal (IdentityCharacter you) excess
      pure e
    _ -> MomentOfTriumph <$> runMessage msg attrs
