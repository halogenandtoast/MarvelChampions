module Marvel.SideScheme.SideSchemes.DroneFactory (
  droneFactory,
  DroneFactory (..),
) where

import Marvel.Prelude

import Marvel.Id (SchemeId (..))

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

droneFactory :: SideSchemeCard DroneFactory
droneFactory = sideScheme DroneFactory Cards.droneFactory (Static 4)

newtype DroneFactory = DroneFactory (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage DroneFactory where
  runMessage msg s@(DroneFactory attrs) = runQueueT $ case msg of
    -- each player puts a Drone into play, then 1 threat is placed here for each Drone
    -- minion in play, counted once they are
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players putTopOfDeckIntoPlayAsDrone
      sideSchemeRevealedThen ident
      pure s
    SideSchemeMessage ident SideSchemeRevealedThen | ident == sideSchemeId attrs -> do
      drones <- selectListCount (MinionWithTrait Drone)
      placeThreat ident drones
      pure s
    _ -> DroneFactory <$> runMessage msg attrs
