module Marvel.SideScheme.SideSchemes.SmSandman where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Environment.Environments.SmSandman
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype SmSandmanSideScheme = SmSandmanSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smDirtTrap, smTidalSands :: SideSchemeCard SmSandmanSideScheme
smDirtTrap = sideSchemeWith SmSandmanSideScheme Cards.smDirtTrap (Static 1) (crisisL .~ True)
smTidalSands = sideScheme SmSandmanSideScheme Cards.smTidalSands (Static 2)

instance RunMessage SmSandmanSideScheme where
  runMessage msg e@(SmSandmanSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27068" -> do
      surgingSands
      surgingSands
      SmSandmanSideScheme <$> runMessage msg attrs
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27069" -> do
      n <- sandCounters
      placeThreat (sideSchemeId attrs) n
      pure e
    _ -> SmSandmanSideScheme <$> runMessage msg attrs
