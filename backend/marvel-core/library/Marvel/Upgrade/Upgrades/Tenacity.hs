module Marvel.Upgrade.Upgrades.Tenacity
  ( tenacity
  , Tenacity(..)
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
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

tenacity :: UpgradeCard Tenacity
tenacity = upgrade Tenacity Cards.tenacity

newtype Tenacity = Tenacity UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Tenacity where
  getAbilities (Tenacity a) =
    [ ability
        a
        1
        HeroAction
        (OwnsThis <> Exhausted)
        (ResourceCost (Just Physical))
        (Run
          [ RemoveFromPlay (toTarget a)
          , IdentityMessage (upgradeController a) ReadyIdentity
          ]
        )
    ]

instance RunMessage Tenacity where
  runMessage msg (Tenacity attrs) = Tenacity <$> runMessage msg attrs
