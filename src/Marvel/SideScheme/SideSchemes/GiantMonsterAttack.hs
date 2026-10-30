module Marvel.SideScheme.SideSchemes.GiantMonsterAttack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Resource

giantMonsterAttack :: SideSchemeCard GiantMonsterAttack
giantMonsterAttack = sideSchemeWith GiantMonsterAttack Cards.giantMonsterAttack (Static 4) (crisisL .~ True)

newtype GiantMonsterAttack = GiantMonsterAttack (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor GiantMonsterAttack where
  getModifiersFor _ (SideSchemeRef sid) (GiantMonsterAttack attrs) | sid == sideSchemeId attrs =
    pure [AdditionalThwartCost $ ResourceCost $ Just Energy]
  getModifiersFor _ _ _ = pure []

instance RunMessage GiantMonsterAttack where
  runMessage msg e@(GiantMonsterAttack attrs) = runQueueT $ case msg of
    _ -> GiantMonsterAttack <$> runMessage msg attrs
