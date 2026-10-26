module Marvel.Support.Supports.ServalIndustries (
  servalIndustries,
  ServalIndustries (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

servalIndustries :: SupportCard ServalIndustries
servalIndustries = support ServalIndustries Cards.servalIndustries

newtype ServalIndustries = ServalIndustries (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Alter-Ego Action: Exhaust Serval Industries → shuffle 2 Quicksilver cards from
-- your discard pile into your deck": cards of his hero set, as many as there are up to
-- 2, the player choosing which.
instance HasAbilities ServalIndustries where
  getAbilities a@(ServalIndustries attrs) =
    [ ability
        a
        1
        AlterEgoAction
        ( OwnsThis
            <> ExtendedCardExists (InDiscardOf (IdentityWithId $ supportController attrs) (BasicCardMatches HeroSpecificCard))
        )
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage ServalIndustries where
  runMessage msg s@(ServalIndustries attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch HeroSpecificCard) <$> projectMap PlayerIdentityDiscard unDiscard ident
      if length cards <= 2
        then shuffleIntoDeck ident cards
        else
          chooseOne ident
            [ targeting c $ chooseOne ident [targeting c' (shuffleIntoDeck ident [c, c']) | c' <- cards, c' /= c]
            | c <- cards
            ]
      pure s
    _ -> ServalIndustries <$> runMessage msg attrs
