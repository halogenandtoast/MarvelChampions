module Marvel.Upgrade.Upgrades.MagicShield (
  magicShield,
  MagicShield (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

magicShield :: UpgradeCard MagicShield
magicShield = upgrade MagicShield Cards.magicShield

newtype MagicShield = MagicShield (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When a friendly character would take any amount of damage, discard
-- Magic Shield → prevent 3 of that damage. Any player's hero or ally, from any source.
instance HasAbilities MagicShield where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        ( WindowOneOf
            [ WouldTakeDamage AnyIdentity FromAnyDamageSource (GreaterThan $ Static 0)
            , AllyWouldTakeDamage AnyAlly
            ]
        )
        HeroInterrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

instance RunMessage MagicShield where
  runMessage msg u@(MagicShield attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [IdentityTakeDamage ident _] _ -> do
      preventDamage ident 3
      pure u
    RanAbility _ (isTarget attrs -> True) 1 [AllyTakeDamage aid _] _ -> do
      preventAllyDamage aid 3
      pure u
    _ -> MagicShield <$> runMessage msg attrs
