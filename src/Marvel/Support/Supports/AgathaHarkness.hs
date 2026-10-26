module Marvel.Support.Supports.AgathaHarkness (
  agathaHarkness,
  AgathaHarkness (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

agathaHarkness :: SupportCard AgathaHarkness
agathaHarkness = support AgathaHarkness Cards.agathaHarkness

newtype AgathaHarkness = AgathaHarkness (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: Exhaust Agatha Harkness → look at the top 3 cards of your deck. Add
-- 1 of those to your hand and place the rest on the bottom of your deck in any order.
instance HasAbilities AgathaHarkness where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage AgathaHarkness where
  runMessage msg a@(AgathaHarkness attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 3) AnyCard SearchDrawOne BottomInAnyOrder
      pure a
    _ -> AgathaHarkness <$> runMessage msg attrs
