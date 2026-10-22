module Marvel.Event.Events.ExpertDefense (
  expertDefense,
  ExpertDefense (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Window

expertDefense :: EventCard ExpertDefense
expertDefense = event ExpertDefense Cards.expertDefense

newtype ExpertDefense = ExpertDefense (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Played as the hero defends, before its DEF is applied: the 3 more DEF is 3 less
-- damage from that attack.
instance RunMessage ExpertDefense where
  runMessage msg e@(ExpertDefense attrs) = case msg of
    EventMessage ident (PlayedEvent _ _ (Just (HeroDefends defender _))) | ident == eventId attrs -> do
      push $ IdentityMessage defender $ IdentityDefended 3
      pure e
    _ -> ExpertDefense <$> runMessage msg attrs
