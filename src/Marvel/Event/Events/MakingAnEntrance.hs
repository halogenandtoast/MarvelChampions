module Marvel.Event.Events.MakingAnEntrance (
  makingAnEntrance,
  makingAnEntranceEffect,
  MakingAnEntrance (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

makingAnEntrance :: EventCard MakingAnEntrance
makingAnEntrance = event MakingAnEntrance Cards.makingAnEntrance

newtype MakingAnEntrance = MakingAnEntrance (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | As your hero makes a basic thwart: +2 THW for it, and an effect watching that
-- thwart for the scheme it empties.
instance RunMessage MakingAnEntrance where
  runMessage msg e@(MakingAnEntrance attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      addToBasicPower you 2
      createEffect Cards.makingAnEntrance attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> MakingAnEntrance <$> runMessage msg attrs

-- | "After that thwart ends, if your hero removed all threat from a scheme that way, heal
-- 2 damage from your hero": the heal follows the window after the basic thwart, and the
-- effect ends with that thwart, whatever it did.
newtype MakingAnEntranceEffect = MakingAnEntranceEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

makingAnEntranceEffect :: CardEffect MakingAnEntranceEffect
makingAnEntranceEffect =
  effectWith MakingAnEntranceEffect Cards.makingAnEntrance (endsL ?~ DisableAtEndOfPhase)

instance RunMessage MakingAnEntranceEffect where
  runMessage msg e@(MakingAnEntranceEffect attrs) = runQueueT $ case msg of
    CheckWindows [W.Window W.After (W.ThwartedAllThreat ident _)] -> do
      whenM (effectValidFor attrs (toRef ident)) $
        flip afterMatching (heal ident 2) \case
          CheckWindows [W.Window W.After (W.MadeBasicThwart ident')] -> ident' == ident
          _ -> False
      pure e
    CheckWindows [W.Window W.After (W.MadeBasicThwart ident)] -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    _ -> MakingAnEntranceEffect <$> runMessage msg attrs
