module Marvel.Game.Public (publicGame, publicGameFor) where

import Marvel.Prelude

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Marvel.Game
import Marvel.Id

-- | The game as the players may see it: decks are hidden information, so only their
-- sizes are sent. Stored games and exports keep everything.
publicGame :: Game -> Value
publicGame = publicGameFor (const True)

-- | The game as someone sees it who may only look at the hands of the players given:
-- the rest send only their sizes.
publicGameFor :: (IdentityId -> Bool) -> Game -> Value
publicGameFor seesHand g = hide (toJSON g)
 where
  hide = \case
    Object o ->
      Object
        $ adjust (onObject $ adjust (overEntries \pid -> if seesHand pid then hideDeck else hideHand . hideDeck) "players") "entities"
        $ adjust hideScenario "scenario" o
    v -> v
  hideDeck = withSize "deck" "deckSize"
  hideHand = \case
    Object o | Just (Array cards) <- KeyMap.lookup "hand" o ->
      Object $ KeyMap.insert "handSize" (toJSON $ length cards) $ KeyMap.insert "hand" (Array mempty) o
    v -> v
  hideScenario = withSize "scenarioEncounterDeck" "encounterDeckSize"
  withSize key sizeKey = \case
    Object o | Just (Array cards) <- KeyMap.lookup key o ->
      Object $ KeyMap.insert sizeKey (toJSON $ length cards) $ KeyMap.delete key o
    v -> v
  onObject f = \case
    Object o -> Object (f o)
    v -> v
  adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)
  -- keyed by player
  overEntries f = \case
    Object o -> Object $ KeyMap.mapWithKey (\k -> maybe id f (parseMaybe parseJSON (String $ Key.toText k))) o
    v -> v
