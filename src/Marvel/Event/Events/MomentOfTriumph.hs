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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Game.Source (getCurrentWindows)
import Marvel.Ref
import Marvel.Window qualified as W

momentOfTriumph :: EventCard MomentOfTriumph
momentOfTriumph = event MomentOfTriumph Cards.momentOfTriumph

newtype MomentOfTriumph = MomentOfTriumph (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent MomentOfTriumph where
  eventTargets _ = do
    windows <- getCurrentWindows
    let excess = \case
          W.DefeatedMinion _ _ _ n -> n > 0
          W.DefeatedVillain _ _ _ n -> n > 0
          _ -> False
    pure $ if any (excess . W.windowType) windows then Nothing else Just []

-- | Hero Response: After you attack and defeat an enemy, heal 1 damage from your hero for
-- each point of excess damage dealt to that enemy by that attack.
instance RunMessage MomentOfTriumph where
  runMessage msg e@(MomentOfTriumph attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just window)) | ident == eventId attrs -> do
      let excess = case window of
            W.DefeatedMinion _ _ _ n -> n
            W.DefeatedVillain _ _ _ n -> n
            _ -> 0
      when (excess > 0) $ heal (IdentityCharacter you) excess
      pure e
    _ -> MomentOfTriumph <$> runMessage msg attrs
