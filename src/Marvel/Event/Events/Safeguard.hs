module Marvel.Event.Events.Safeguard (
  safeguard,
  Safeguard (..),
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
import Marvel.Status ()

safeguard :: EventCard Safeguard
safeguard = event Safeguard Cards.safeguard

newtype Safeguard = Safeguard (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Give up to 2 friendly characters each a tough status card, one at a time until the
-- player is done, then draw 1 card. A character already tough is no valid target: it
-- cannot have a second (RR Status Cards).
instance RunMessage Safeguard where
  runMessage msg e@(Safeguard attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      cardSpecific attrs "give" (you, 2 :: Natural)
      drawCards you 1
      pure e
    CardSpecific (isTarget attrs -> True) "give" (fromJSON -> Success (you, n :: Natural)) -> do
      tough <- selectList (CharacterWithStatus ToughStatus)
      characters <- filter (`notElem` tough) <$> selectList (CharacterControlledBy AnyIdentity)
      when (n > 0 && notNull characters) $
        chooseOnePrompted you "$label.giveTough" $
          labeled "$label.done" (pure ())
            : [ targeting (characterRef c) do
                  giveStatus c ToughStatus
                  cardSpecific attrs "give" (you, n - 1)
              | c <- characters :: [CharacterId]
              ]
      pure e
    _ -> Safeguard <$> runMessage msg attrs
