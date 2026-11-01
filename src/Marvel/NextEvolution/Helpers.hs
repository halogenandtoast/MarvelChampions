module Marvel.NextEvolution.Helpers where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

routedCount :: HasGame m => m Natural
routedCount = do
  routed <- selectList (EnvironmentWithTitle "Routed")
  fromIntegral . length . concat <$> traverse heldCardsAt routed
