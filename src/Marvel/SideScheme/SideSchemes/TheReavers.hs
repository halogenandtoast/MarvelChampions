module Marvel.SideScheme.SideSchemes.TheReavers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.GameValue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theReavers :: SideSchemeCard TheReavers
theReavers = sideScheme TheReavers Cards.theReavers (Static 5)

newtype TheReavers = TheReavers (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheReavers where
  runMessage msg s@(TheReavers attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      discardUntil FromEncounterDeck (CardWithTrait Trait.Reaver <> CardWithType MinionType) attrs
      pure s
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- maybe getYou pure (sideSchemeDefeatedBy attrs)
      revealEncounterCard you card
      pure s
    _ -> TheReavers <$> runMessage msg attrs
