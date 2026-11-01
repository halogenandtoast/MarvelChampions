module Marvel.Event.Events.BreakTime where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Game.Source (getPlayers)
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

breakTime :: EventCard BreakTime
breakTime = event BreakTime Cards.breakTime

newtype BreakTime = BreakTime (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BreakTime where
  runMessage msg e@(BreakTime attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      cardSpecific attrs "minutes" (0 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "minutes" (fromJSON -> Success (minutes :: Natural)) -> do
      chooseOnePrompted (eventController attrs) (countLabel "breakTimeMinutes" minutes)
        [ labeled "$label.breakTimeOneMinute" (cardSpecific attrs "minutes" (minutes + 1))
        , labeled "$label.breakTimeFiveMinutes" (cardSpecific attrs "minutes" (minutes + 5))
        , labeled "$label.breakTimeFifteenMinutes" (cardSpecific attrs "minutes" (minutes + 15))
        , labeled "$label.breakTimeSixtyMinutes" (cardSpecific attrs "minutes" (minutes + 60))
        , labeled "$label.breakTimeResume" (cardSpecific attrs "resume" minutes)
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "resume" (fromJSON -> Success (minutes :: Natural)) -> do
      players <- getPlayers
      for_ players $ \you -> heal you minutes
      pure e
    _ -> BreakTime <$> runMessage msg attrs
