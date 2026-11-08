module Marvel.Support.Supports.StevesApartment (
  stevesApartment,
  StevesApartment (..),
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

stevesApartment :: SupportCard StevesApartment
stevesApartment = support StevesApartment Cards.stevesApartment

newtype StevesApartment = StevesApartment (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StevesApartment where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage StevesApartment where
  runMessage msg s@(StevesApartment attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      drawCards you 1
      heal you 1
      pure s
    _ -> StevesApartment <$> runMessage msg attrs
