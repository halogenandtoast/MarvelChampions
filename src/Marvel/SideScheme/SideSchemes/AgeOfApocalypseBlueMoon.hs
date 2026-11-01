module Marvel.SideScheme.SideSchemes.AgeOfApocalypseBlueMoon where

import Marvel.Prelude
import Marvel.Minion.Minions.AgeOfApocalypseGenosha ()
import Marvel.Ability ()
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.AgeOfApocalypse.Settings ()
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Ref

import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait qualified as Trait

newtype AoaBlueMoonSideScheme = AoaBlueMoonSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

trialByCombat :: SideSchemeCard AoaBlueMoonSideScheme
trialByCombat = sideScheme AoaBlueMoonSideScheme Cards.trialByCombat (PerPlayer 2)

instance RunMessage AoaBlueMoonSideScheme where
  runMessage msg e@(AoaBlueMoonSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      cards <- filter (cardMatch $ CardWithType MinionType <> CardWithTrait Trait.ImperialGuard) <$> getEncounterDiscard
      shuffleIntoEncounterDeck cards
      pure e
    _ -> AoaBlueMoonSideScheme <$> runMessage msg attrs
