module Marvel.Support.Supports.OrorosGarden where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Question
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

ororosGarden :: SupportCard OrorosGarden
ororosGarden = support OrorosGarden Cards.ororosGarden

newtype OrorosGarden = OrorosGarden (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OrorosGarden where
  getAbilities a = [ability a 1 AlterEgoAction (OwnsThis <> SelfMatches IdentityWithAnyDamage) ExhaustCost (runAbility a 1)]

instance RunMessage OrorosGarden where
  runMessage msg a@(OrorosGarden attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ healFrom attrs you 2
    _ -> OrorosGarden <$> runMessage msg attrs
