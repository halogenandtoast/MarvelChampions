module Marvel.SideScheme.SideSchemes.AgeOfApocalypseSavageLand where

import Marvel.Prelude
import Marvel.Minion.Minions.AgeOfApocalypseGenosha ()
import Marvel.Trait ()
import Marvel.Ability ()
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Ref

import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype AoaSavageLandSideScheme = AoaSavageLandSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

villageUnderAttack :: SideSchemeCard AoaSavageLandSideScheme
villageUnderAttack = sideScheme AoaSavageLandSideScheme Cards.villageUnderAttack (PerPlayer 2)

instance RunMessage AoaSavageLandSideScheme where
  runMessage msg e@(AoaSavageLandSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      getPlayers >>= traverse_ resolveSettingSpecial
      pure e
    _ -> AoaSavageLandSideScheme <$> runMessage msg attrs
