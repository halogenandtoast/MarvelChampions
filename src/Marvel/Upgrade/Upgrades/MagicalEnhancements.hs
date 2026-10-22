module Marvel.Upgrade.Upgrades.MagicalEnhancements (
  magicalEnhancements,
  MagicalEnhancements (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

magicalEnhancements :: UpgradeCard MagicalEnhancements
magicalEnhancements = upgrade MagicalEnhancements Cards.magicalEnhancements

newtype MagicalEnhancements = MagicalEnhancements (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Your hero gets +1 THW, +1 ATK and +1 DEF.
instance HasModifiersFor MagicalEnhancements where
  getModifiersFor _ (IdentityRef ident) (MagicalEnhancements a)
    | ident == upgradeController a =
        pure [ThwartModifier 1, AttackModifier 1, DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Forced Interrupt: when the round ends, discard it.
instance HasAbilities MagicalEnhancements where
  getAbilities (MagicalEnhancements a) =
    [windowAbility a 1 W.RoundEnds ForcedInterrupt NoCost $ DiscardTarget (toRef a)]

instance RunMessage MagicalEnhancements where
  runMessage msg (MagicalEnhancements attrs) = MagicalEnhancements <$> runMessage msg attrs
