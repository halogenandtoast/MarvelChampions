module Marvel.SideScheme.SideSchemes.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Ability.Types ()
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype SmVenomGoblinSideScheme = SmVenomGoblinSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smFesteringMass :: SideSchemeCard SmVenomGoblinSideScheme
smFesteringMass = sideSchemeWith SmVenomGoblinSideScheme Cards.smFesteringMass (Static 5) (crisisL .~ False)

smJoyRide :: SideSchemeCard SmVenomGoblinSideScheme
smJoyRide = sideSchemeWith SmVenomGoblinSideScheme Cards.smJoyRide (Static 2) (crisisL .~ False)

instance RunMessage SmVenomGoblinSideScheme where
  runMessage msg e@(SmVenomGoblinSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27125" -> do
      you <- getYou
      Goblin.chooseGliderScheme True you "goblinSpecialSelected"
      SmVenomGoblinSideScheme <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27124" -> do
      getBoostedPlayer enemy >>= revealBoost attrs
      pure e
    _ -> SmVenomGoblinSideScheme <$> runMessage msg attrs
