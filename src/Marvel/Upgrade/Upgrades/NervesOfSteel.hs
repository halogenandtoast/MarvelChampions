module Marvel.Upgrade.Upgrades.NervesOfSteel (
  nervesOfSteel,
  NervesOfSteel (..),
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

nervesOfSteel :: UpgradeCard NervesOfSteel
nervesOfSteel = upgrade NervesOfSteel Cards.nervesOfSteel

newtype NervesOfSteel = NervesOfSteel (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Resource: Exhaust Nerves of Steel → generate a [energy] resource for a [[Defense]]
-- event."
instance HasAbilities NervesOfSteel where
  getAbilities (NervesOfSteel a) =
    [ ability
        a
        1
        Resource
        (OwnsThis <> PayingFor (CardWithType EventType <> CardWithTrait Trait.Defense))
        ExhaustCost
        (Pay $ ResourcePayment Energy)
    ]

instance RunMessage NervesOfSteel where
  runMessage msg (NervesOfSteel attrs) = NervesOfSteel <$> runMessage msg attrs
