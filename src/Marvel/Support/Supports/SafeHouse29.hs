module Marvel.Support.Supports.SafeHouse29 (
  safeHouse29,
  SafeHouse29 (..),
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
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

safeHouse29 :: SupportCard SafeHouse29
safeHouse29 = support SafeHouse29 Cards.safeHouse29

newtype SafeHouse29 = SafeHouse29 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

preparation :: CardMatcher
preparation = CardWithTrait Preparation

instance HasAbilities SafeHouse29 where
  getAbilities a@(SafeHouse29 attrs) =
    [ ability
        a
        1
        AlterEgoAction
        (ExtendedCardExists $ InDiscardOf (IdentityWithId $ supportController attrs) (BasicCardMatches preparation))
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage SafeHouse29 where
  runMessage msg s@(SafeHouse29 attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch preparation) <$> projectMap PlayerIdentityDiscard unDiscard ident
      push $
        Ask ident $
          ChooseOne [TargetLabel (toRef c) [Run [IdentityMessage ident (AddToHand c)]] | c <- cards]
      pure s
    _ -> SafeHouse29 <$> runMessage msg attrs
