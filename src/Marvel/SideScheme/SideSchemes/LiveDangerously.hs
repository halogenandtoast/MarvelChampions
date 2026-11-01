module Marvel.SideScheme.SideSchemes.LiveDangerously where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

liveDangerously :: SideSchemeCard LiveDangerously
liveDangerously = sideSchemeWith LiveDangerously Cards.liveDangerously (PerPlayer 3) (\a -> a {sideSchemeCrisis = True})

newtype LiveDangerously = LiveDangerously (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LiveDangerously where
  getModifiersFor _ (IdentityRef _) _ = pure [HandSizeModifier 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage LiveDangerously where
  runMessage msg (LiveDangerously attrs) = runQueueT $ LiveDangerously <$> runMessage msg attrs
