module Marvel.AlterEgo.AlterEgos.JubilationLee where
import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats

jubilationLee :: AlterEgoCard JubilationLee
jubilationLee = alterEgo JubilationLee Cards.jubilee47001b (HP $ Static 9) (HandSize 6) (Rec 3) [Cards.jubilee47023]
newtype JubilationLee = JubilationLee (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
instance HasAbilities JubilationLee where
  getAbilities a = [label "$label.jubileeMallRat" $ limitedAbility a 1 (PerPhase 1) Action
    (IsSelf <> ExtendedCardExists (InDeckOf You (BasicCardMatches $ CardIs Cards.jubilee47003))) NoCost (runAbility a 1)]
instance RunMessage JubilationLee where
  runMessage msg a@(JubilationLee attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch $ CardIs Cards.jubilee47003) <$> projectMap PlayerIdentityDeck unDeck you
      chooseOrRunOne you [targeting card $ putIntoPlay you card | card <- cards]
      shuffleDeck you
      pure a
    _ -> JubilationLee <$> runMessage msg attrs
