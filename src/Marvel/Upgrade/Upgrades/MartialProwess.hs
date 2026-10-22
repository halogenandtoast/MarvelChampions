module Marvel.Upgrade.Upgrades.MartialProwess (
  martialProwess,
  MartialProwess (..),
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
import Marvel.Card.Def (CardMatcher (..), CardType (EventType))
import Marvel.Resource
import Marvel.Trait qualified as Trait

martialProwess :: UpgradeCard MartialProwess
martialProwess = upgrade MartialProwess Cards.martialProwess

newtype MartialProwess = MartialProwess (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Resource: Exhaust Martial Prowess → generate a [physical] resource for an
-- [[Attack]] event."
instance HasAbilities MartialProwess where
  getAbilities (MartialProwess a) =
    [ ability
        a
        1
        Resource
        (OwnsThis <> PayingFor (CardWithType EventType <> CardWithTrait Trait.Attack))
        ExhaustCost
        (Pay $ ResourcePayment Physical)
    ]

instance RunMessage MartialProwess where
  runMessage msg (MartialProwess attrs) = MartialProwess <$> runMessage msg attrs
