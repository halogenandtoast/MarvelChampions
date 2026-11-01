module Marvel.Upgrade.Upgrades.ExpertMarksman (
  expertMarksman,
  ExpertMarksman (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Payment ()
import Marvel.Resource
import Marvel.Trait (Trait (Arrow))

expertMarksman :: UpgradeCard ExpertMarksman
expertMarksman = upgrade ExpertMarksman Cards.expertMarksman

newtype ExpertMarksman = ExpertMarksman (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Resource: Exhaust Expert Marksman → generate a [wild] resource for an Arrow event.
instance HasAbilities ExpertMarksman where
  getAbilities a =
    [ability a 1 Resource (OwnsThis <> PayingFor (CardWithType EventType <> CardWithTrait Arrow)) ExhaustCost $ Pay (ResourcePayment Wild)]

instance RunMessage ExpertMarksman where
  runMessage msg (ExpertMarksman attrs) = ExpertMarksman <$> runMessage msg attrs
