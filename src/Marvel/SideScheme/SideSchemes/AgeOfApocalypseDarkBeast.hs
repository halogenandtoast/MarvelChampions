module Marvel.SideScheme.SideSchemes.AgeOfApocalypseDarkBeast where

import Marvel.Prelude
import Marvel.Minion.Minions.AgeOfApocalypseGenosha ()
import Marvel.Trait ()
import Marvel.Ability ()
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.Game.Source ()
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref

import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype AoaDarkBeastSideScheme = AoaDarkBeastSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

timeTravelShenanigans :: SideSchemeCard AoaDarkBeastSideScheme
timeTravelShenanigans = sideScheme AoaDarkBeastSideScheme Cards.timeTravelShenanigans (PerPlayer 2)

instance RunMessage AoaDarkBeastSideScheme where
  runMessage msg e@(AoaDarkBeastSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      setting <- currentSetting
      for_ setting $ \(_, def) -> for_ (cdEncounterSet def) $ \encounterSet ->
        discardUntil FromEncounterDeck (CardInEncounterSet encounterSet) attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- getDefeatedBy attrs
      revealEncounterCard you card
      pure e
    _ -> AoaDarkBeastSideScheme <$> runMessage msg attrs
