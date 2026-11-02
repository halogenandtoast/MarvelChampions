module Marvel.SideScheme.SideSchemes.TheGreatHuntSilk where
import Marvel.Prelude
import Marvel.Id ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

theGreatHuntSilk :: SideSchemeCard TheGreatHuntSilk
theGreatHuntSilk = sideScheme TheGreatHuntSilk Cards.theGreatHuntSilk (Static 2)
newtype TheGreatHuntSilk = TheGreatHuntSilk (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TheGreatHuntSilk where
  runMessage msg u@(TheGreatHuntSilk attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      n <- sum <$> traverse (fmap (fromIntegral . length) . tuckedEncounterCards) players
      placeThreatOn attrs sid n
      pure u
    _ -> TheGreatHuntSilk <$> runMessage msg attrs
