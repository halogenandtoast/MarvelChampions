module Marvel.SideScheme.SideSchemes.DroneFactory (
  droneFactory,
  DroneFactory (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

droneFactory :: SideSchemeCard DroneFactory
droneFactory = sideScheme DroneFactory Cards.droneFactory (Static 4)

newtype DroneFactory = DroneFactory (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage DroneFactory where
  runMessage msg s@(DroneFactory attrs) = case msg of
    -- each player puts a Drone into play, then 1 threat is placed here for each Drone
    -- minion in play
    -- ponytail: counted as the drones now in play plus one for each player, as each
    -- player is about to add one; a player with no cards left to make one is not seen
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      drones <- selectListCount (MinionWithTrait Drone)
      pushAll $
        map PutTopOfDeckIntoPlayAsDrone players
          <> [SideSchemeMessage ident $ SideSchemePlaceThreat (drones + fromIntegral (length players))]
      pure s
    _ -> DroneFactory <$> runMessage msg attrs
