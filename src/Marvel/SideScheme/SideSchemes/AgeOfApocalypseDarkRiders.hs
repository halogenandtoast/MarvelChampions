module Marvel.SideScheme.SideSchemes.AgeOfApocalypseDarkRiders where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaDarkRidersSideScheme = AoaDarkRidersSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

theDarkRiders :: SideSchemeCard AoaDarkRidersSideScheme
theDarkRiders = sideScheme AoaDarkRidersSideScheme Cards.theDarkRiders (Static 2)

instance HasModifiersFor AoaDarkRidersSideScheme where
  getModifiersFor _ (MinionRef mid) _ = do
    rider <- selectAny $ MinionWithId mid <> MinionWithTrait Trait.DarkRiders
    pure [KeywordModifier Toughness | rider]
  getModifiersFor _ _ _ = pure []
instance RunMessage AoaDarkRidersSideScheme where
  runMessage msg a@(AoaDarkRidersSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Trait.DarkRiders) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- getYou
      revealEncounterCard you card
      pure a
    _ -> AoaDarkRidersSideScheme <$> runMessage msg attrs
