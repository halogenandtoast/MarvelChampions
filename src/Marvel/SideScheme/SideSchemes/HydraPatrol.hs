module Marvel.SideScheme.SideSchemes.HydraPatrol (
  hydraPatrol,
  HydraPatrol (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

hydraPatrol :: SideSchemeCard HydraPatrol
hydraPatrol = sideSchemeWith HydraPatrol Cards.hydraPatrol (PerPlayer 2) (crisisL .~ True)

newtype HydraPatrol = HydraPatrol (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: each player puts a Hydra minion from the encounter deck or discard
-- pile into play engaged with them, the deck shuffled.
instance RunMessage HydraPatrol where
  runMessage msg s@(HydraPatrol attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> searchAndPutIntoPlayEncounterCard p (CardWithType MinionType <> CardWithTrait Hydra)
      pure s
    _ -> HydraPatrol <$> runMessage msg attrs
