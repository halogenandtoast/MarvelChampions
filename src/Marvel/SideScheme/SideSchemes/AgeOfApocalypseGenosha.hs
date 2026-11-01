module Marvel.SideScheme.SideSchemes.AgeOfApocalypseGenosha where

import Marvel.Prelude
import Marvel.Trait ()
import Marvel.Ability ()
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.Game.Source ()
import Marvel.AgeOfApocalypse.Settings ()
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref

import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Minion.Minions.AgeOfApocalypseGenosha (findAndRevealEscapedMutant)

newtype AoaGenoshaSideScheme = AoaGenoshaSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

policeState :: SideSchemeCard AoaGenoshaSideScheme
policeState = sideScheme AoaGenoshaSideScheme Cards.policeState (PerPlayer 2)

instance RunMessage AoaGenoshaSideScheme where
  runMessage msg e@(AoaGenoshaSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      getDefeatedBy attrs >>= findAndRevealEscapedMutant
      pure e
    _ -> AoaGenoshaSideScheme <$> runMessage msg attrs
