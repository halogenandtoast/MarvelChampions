module Marvel.Upgrade.Upgrades.Finesse (
  finesse,
  Finesse (..),
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
import Marvel.Aspect
import Marvel.Payment ()
import Marvel.Resource

finesse :: UpgradeCard Finesse
finesse = upgrade Finesse Cards.finesse

newtype Finesse = Finesse (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Resource: Exhaust Finesse → generate a [wild] resource for an aspect card; its
-- cost or a cost within its ability (FAQ, Finesse #33).
instance HasAbilities Finesse where
  getAbilities a =
    [ ability a 1 HeroResource (OwnsThis <> PayingFor (CardMatchesAny $ map CardWithAspect [Aggression, Justice, Leadership, Protection, Pool])) ExhaustCost $
        Pay (ResourcePayment Wild)
    ]

instance RunMessage Finesse where
  runMessage msg (Finesse attrs) = Finesse <$> runMessage msg attrs
