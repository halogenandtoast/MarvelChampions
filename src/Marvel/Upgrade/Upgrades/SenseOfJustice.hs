module Marvel.Upgrade.Upgrades.SenseOfJustice (
  senseOfJustice,
  SenseOfJustice (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def (CardMatcher (..), CardType (EventType))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

senseOfJustice :: UpgradeCard SenseOfJustice
senseOfJustice = upgrade SenseOfJustice Cards.senseOfJustice

newtype SenseOfJustice = SenseOfJustice (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Resource: Exhaust Sense of Justice → generate a [mental] resource for a [[Thwart]]
-- event." Played under any player's control, max 1 per player: its card limit.
instance HasAbilities SenseOfJustice where
  getAbilities (SenseOfJustice a) =
    [ ability
        a
        1
        Resource
        (OwnsThis <> PayingFor (CardWithType EventType <> CardWithTrait Trait.Thwart))
        ExhaustCost
        (Pay $ ResourcePayment Mental)
    ]

instance RunMessage SenseOfJustice where
  runMessage msg (SenseOfJustice attrs) = SenseOfJustice <$> runMessage msg attrs
