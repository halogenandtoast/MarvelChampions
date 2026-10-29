module Marvel.Support.Supports.TheBifrost (theBifrost, TheBifrost (..)) where

import Marvel.Prelude
import Marvel.Question
import Marvel.Deck (unDeck)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

-- PlayingCard preserves the deck as the source zone: it pays ordinary costs,
-- checks uniqueness and emits ordinary play/enter-play windows. The subsequent
-- shuffle waits until that play (including its payment and responses) finishes.
theBifrost :: SupportCard TheBifrost
theBifrost = support TheBifrost Cards.theBifrost

newtype TheBifrost = TheBifrost (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheBifrost where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage TheBifrost where
  runMessage msg s@(TheBifrost attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      playable <- selectList $ AffordableCardBy (IdentityWithId you)
        (BasicCardMatches $ CardWithType AllyType <> CardWithTrait Asgard)
      let found = filter (`elem` playable) deck
      unless (null found) $ lookingAt you (map PlayerCard deck) $ chooseOne you
        [ targeting (toRef card) (identityStep you $ PlayingCard card Nothing)
        | card <- found
        ]
      shuffleDeck you
      pure s
    _ -> TheBifrost <$> runMessage msg attrs
