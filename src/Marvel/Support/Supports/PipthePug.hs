module Marvel.Support.Supports.PipthePug where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

pipthePug :: SupportCard PipthePug
pipthePug = support PipthePug Cards.pipthePug
newtype PipthePug = PipthePug (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PipthePug where
  getAbilities a = [ability a 1 AlterEgoAction
    (OwnsThis <> ExtendedCardExists (InDiscardOf You (BasicCardMatches (CardMatchesAny [CardWithTrait Trait.Posse, CardWithTitle "Diamondback", CardWithTitle "Outlaw", CardWithTitle "A Good Workout", CardWithTitle "Luck Be a Lady", CardWithTitle "Right Place, Right Time", CardWithTitle "Jackpot!", CardWithTitle "Pip the Pug", CardWithTitle "The Painted Lady", CardWithTitle "Domino's Pistol", CardWithTitle "Lucky and Good", CardWithTitle "Lucky Break", CardWithTitle "Probability Field"]))))
    ExhaustCost (runAbility a 1)]

instance RunMessage PipthePug where
  runMessage msg a@(PipthePug attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- projectMap PlayerIdentityDiscard unDiscard you
      let eligible card = cdCardCode (getCardDef card) `elem` ["40038", "40039", "40040", "40041", "40042", "40043", "40044", "40045", "40046", "40047", "40048", "40049"] || cardMatch (CardWithTrait Trait.Posse) card
      chooseOne you [targeting card (placeOnTopOfDeck you card) | card <- cards, eligible card]
      pure a
    _ -> PipthePug <$> runMessage msg attrs
