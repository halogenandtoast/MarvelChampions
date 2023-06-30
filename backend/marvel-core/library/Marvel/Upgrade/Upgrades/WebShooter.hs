module Marvel.Upgrade.Upgrades.WebShooter (
  webShooter,
  WebShooter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
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

webShooter :: UpgradeCard WebShooter
webShooter =
  upgradeWith WebShooter Cards.webShooter $
    (usesL .~ 3)
      . (discardIfNoUsesL .~ True)

newtype WebShooter = WebShooter (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WebShooter where
  getAbilities (WebShooter a) =
    [ ability
        a
        1
        Resource
        (OwnsThis <> InHeroForm)
        (ExhaustCost <> UseCost)
        (Pay $ ResourcePayment Wild)
    ]

instance RunMessage WebShooter where
  runMessage msg (WebShooter attrs) = WebShooter <$> runMessage msg attrs
