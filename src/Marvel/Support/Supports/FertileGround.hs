module Marvel.Support.Supports.FertileGround (
  fertileGround,
  FertileGround (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

fertileGround :: SupportCard FertileGround
fertileGround = support FertileGround Cards.fertileGround

newtype FertileGround = FertileGround (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: Exhaust Fertile Ground → place 1 growth counter on Groot (to a
-- maximum of 10) and draw 1 card.
instance HasAbilities FertileGround where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility a 1]

instance RunMessage FertileGround where
  runMessage msg s@(FertileGround attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      placeGrowthCounters ident 1
      drawCards ident 1
      pure s
    _ -> FertileGround <$> runMessage msg attrs
