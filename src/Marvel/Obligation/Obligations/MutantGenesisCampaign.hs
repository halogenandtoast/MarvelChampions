module Marvel.Obligation.Obligations.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource

mgReactiveDefense :: ObligationCard MutantGenesisCampaignObligation
mgReactiveDefense = obligation MutantGenesisCampaignObligation Cards.mgReactiveDefense

newtype MutantGenesisCampaignObligation = MutantGenesisCampaignObligation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MutantGenesisCampaignObligation where
  getAbilities a = [ability a 1 AlterEgoAction NoCriteria (MultiResourceCost [Just Energy,Just Mental,Just Physical]) (runAbility a 1)]

instance RunMessage MutantGenesisCampaignObligation where
  runMessage msg o@(MutantGenesisCampaignObligation attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      dealDamageToEach enemies attrs (toDamage 5 FromAbility)
      removeFromGame attrs
      pure o
    _ -> MutantGenesisCampaignObligation <$> runMessage msg attrs
