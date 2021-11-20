module Marvel.Upgrade.Upgrades.WebShooter
  ( webShooter
  , WebShooter(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.Cost
import Marvel.Entity
import Marvel.Message
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import qualified Marvel.Upgrade.Cards as Cards

webShooter :: UpgradeCard WebShooter
webShooter =
  upgradeWith WebShooter Cards.webShooter
    $ (usesL .~ 3)
    . (discardIfNoUsesL .~ True)

newtype WebShooter = WebShooter UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities WebShooter where
  getAbilities (WebShooter a) =
    [ ability
        a
        1
        Resource
        OwnsThis
        (ExhaustCost <> UseCost)
        (Pay $ ResourcePayment Wild)
    ]


instance RunMessage WebShooter where
  runMessage msg (WebShooter attrs) = WebShooter <$> runMessage msg attrs
