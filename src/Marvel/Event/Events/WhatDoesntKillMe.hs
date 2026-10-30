module Marvel.Event.Events.WhatDoesntKillMe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

whatDoesntKillMe :: EventCard WhatDoesntKillMe
whatDoesntKillMe = event WhatDoesntKillMe Cards.whatDoesntKillMe

newtype WhatDoesntKillMe = WhatDoesntKillMe (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WhatDoesntKillMe where
  runMessage msg e@(WhatDoesntKillMe attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> e <$ ready you
    _ -> WhatDoesntKillMe <$> runMessage msg attrs
