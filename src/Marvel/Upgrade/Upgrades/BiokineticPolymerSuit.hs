module Marvel.Upgrade.Upgrades.BiokineticPolymerSuit (
  biokineticPolymerSuit,
  BiokineticPolymerSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

biokineticPolymerSuit :: UpgradeCard BiokineticPolymerSuit
biokineticPolymerSuit = upgrade BiokineticPolymerSuit Cards.biokineticPolymerSuit

newtype BiokineticPolymerSuit = BiokineticPolymerSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BiokineticPolymerSuit where
  getAbilities (BiokineticPolymerSuit a) =
    [ ability
        a
        1
        HeroResource
        (OwnsThis <> PayingFor (CardWithType EventType))
        ExhaustCost
        (Pay $ ResourcePayment Wild)
    ]

instance RunMessage BiokineticPolymerSuit where
  runMessage msg (BiokineticPolymerSuit attrs) =
    BiokineticPolymerSuit <$> runMessage msg attrs
