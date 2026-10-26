module Marvel.Upgrade.Upgrades.ReinforcedSinew (
  reinforcedSinew,
  ReinforcedSinew (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

reinforcedSinew :: UpgradeCard ReinforcedSinew
reinforcedSinew = upgrade ReinforcedSinew Cards.reinforcedSinew

newtype ReinforcedSinew = ReinforcedSinew (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Quicksilver gets +1 ATK."
instance HasModifiersFor ReinforcedSinew where
  getModifiersFor _ (IdentityRef ident) (ReinforcedSinew a)
    | ident == upgradeController a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage ReinforcedSinew where
  runMessage msg (ReinforcedSinew attrs) = ReinforcedSinew <$> runMessage msg attrs
