module Marvel.SideScheme.SideSchemes.TestSubjects (
  testSubjects,
  TestSubjects (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

testSubjects :: SideSchemeCard TestSubjects
testSubjects = sideScheme TestSubjects Cards.testSubjects (PerPlayer 2)

newtype TestSubjects = TestSubjects (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: the first player discards cards from the top of the encounter deck
-- until they discard a minion, and reveals it.
instance RunMessage TestSubjects where
  runMessage msg s@(TestSubjects attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      discardUntil FromEncounterDeck (CardWithType MinionType) attrs
      pure s
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      revealEncounterCard firstPlayer card
      pure s
    _ -> TestSubjects <$> runMessage msg attrs
