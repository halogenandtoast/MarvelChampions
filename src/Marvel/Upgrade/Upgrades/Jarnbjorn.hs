module Marvel.Upgrade.Upgrades.Jarnbjorn (
  jarnbjorn,
  Jarnbjorn (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

jarnbjorn :: UpgradeCard Jarnbjorn
jarnbjorn = upgrade Jarnbjorn Cards.jarnbjorn

newtype Jarnbjorn = Jarnbjorn (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After each enemy the hero attacks, an attack event's included (FAQ).
instance HasAbilities Jarnbjorn where
  getAbilities (Jarnbjorn a) =
    [ limitedWindowAbility
        a
        1
        (W.IdentityAttacked W.After (IdentityWithId $ upgradeController a) AnyEnemy)
        Response
        OwnsThis
        (ResourceCost $ Just Physical)
        (ChooseDamage (toSource a) (toDamage 2 FromAbility) AnyEnemy)
    ]

instance RunMessage Jarnbjorn where
  runMessage msg (Jarnbjorn attrs) = Jarnbjorn <$> runMessage msg attrs
