module Marvel.Upgrade.Upgrades.DefensiveStance (
  defensiveStance,
  DefensiveStance (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

defensiveStance :: UpgradeCard DefensiveStance
defensiveStance = upgrade DefensiveStance Cards.defensiveStance

newtype DefensiveStance = DefensiveStance (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DefensiveStance where
  getAbilities a@(DefensiveStance attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue)
        HeroInterrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

instance RunMessage DefensiveStance where
  runMessage msg u@(DefensiveStance attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      preventDamage ident 3
      pure u
    _ -> DefensiveStance <$> runMessage msg attrs
