module Marvel.Upgrade.Upgrades.Resourceful (
  resourceful,
  Resourceful (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Resource

resourceful :: UpgradeCard Resourceful
resourceful = upgrade Resourceful Cards.resourceful

newtype Resourceful = Resourceful (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Resource: Discard Resourceful → generate a [wild] resource."
instance HasAbilities Resourceful where
  getAbilities (Resourceful a) =
    [ability a 1 Resource OwnsThis (DiscardCost $ toTarget a) (Pay $ ResourcePayment Wild)]

instance RunMessage Resourceful where
  runMessage msg (Resourceful attrs) = Resourceful <$> runMessage msg attrs
