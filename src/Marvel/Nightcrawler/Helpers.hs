module Marvel.Nightcrawler.Helpers where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Id

-- Defined base powers, including an X whose value is defined by a card.
basePowers :: HasGame m => CharacterId -> m (Natural, Natural)
basePowers = getCharacterBasePowers

currentThwart :: HasGame m => CharacterId -> m Natural
currentThwart = getCharacterThwart
