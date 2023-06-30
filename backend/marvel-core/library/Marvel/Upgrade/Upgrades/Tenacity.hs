module Marvel.Upgrade.Upgrades.Tenacity (
  tenacity,
  Tenacity (..),
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
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

tenacity :: UpgradeCard Tenacity
tenacity = upgrade Tenacity Cards.tenacity

newtype Tenacity = Tenacity (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Tenacity where
  getAbilities (Tenacity a) =
    [ ability
        a
        1
        HeroAction
        (OwnsThis <> Exhausted)
        (ResourceCost (Just Physical))
        ( Run
            [ RemoveFromPlay (toTarget a)
            , IdentityMessage (upgradeController a) ReadiedIdentity
            ]
        )
    ]

instance RunMessage Tenacity where
  runMessage msg (Tenacity attrs) = Tenacity <$> runMessage msg attrs
