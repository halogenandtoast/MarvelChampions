module Marvel.SideScheme.SideSchemes.HeraldOfAvalon where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Minion.Cards qualified as Minions

heraldOfAvalon :: SideSchemeCard HeraldOfAvalon
heraldOfAvalon = sideScheme HeraldOfAvalon Cards.heraldOfAvalon (Static 6)

newtype HeraldOfAvalon = HeraldOfAvalon (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HeraldOfAvalon where
  runMessage msg s@(HeraldOfAvalon attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getYou pure (sideSchemeDefeatedBy attrs)
      searchAndRevealEncounterCard you (CardIs Minions.exodus)
      pure s
    _ -> HeraldOfAvalon <$> runMessage msg attrs
