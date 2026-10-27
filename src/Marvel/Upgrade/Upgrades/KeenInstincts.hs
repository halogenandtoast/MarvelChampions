module Marvel.Upgrade.Upgrades.KeenInstincts (
  keenInstincts,
  KeenInstincts (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

keenInstincts :: UpgradeCard KeenInstincts
keenInstincts = upgrade KeenInstincts Cards.keenInstincts

newtype KeenInstincts = KeenInstincts (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Resource: exhaust → a [wild] resource for an Attack or Thwart event.
instance HasAbilities KeenInstincts where
  getAbilities (KeenInstincts a) =
    [ ability
        a
        1
        Resource
        ( OwnsThis
            <> PayingFor (CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Thwart])
        )
        ExhaustCost
        (Pay $ ResourcePayment Wild)
    ]

instance RunMessage KeenInstincts where
  runMessage msg (KeenInstincts attrs) = KeenInstincts <$> runMessage msg attrs
