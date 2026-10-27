module Marvel.SideScheme.SideSchemes.CullTheWeak (
  cullTheWeak,
  CullTheWeak (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 2 per player.
cullTheWeak :: SideSchemeCard CullTheWeak
cullTheWeak = sideScheme CullTheWeak Cards.cullTheWeak (PerPlayer 2)

newtype CullTheWeak = CullTheWeak (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Each enemy gets +2 ATK.
instance HasModifiersFor CullTheWeak where
  getModifiersFor _ target _ = pure case target of
    VillainRef _ -> [AttackModifier 2]
    MinionRef _ -> [AttackModifier 2]
    _ -> []

instance RunMessage CullTheWeak where
  runMessage msg (CullTheWeak attrs) = runQueueT $ CullTheWeak <$> runMessage msg attrs
