module Marvel.SideScheme.SideSchemes.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype MojoSideScheme = MojoSideScheme (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39031 :: SideSchemeCard MojoSideScheme
mojo39031 = sideScheme MojoSideScheme Cards.mojo39031 (Static 2)

instance IsSideScheme MojoSideScheme where
  sideSchemeAbilities = getAbilities

instance HasAbilities MojoSideScheme where
  getAbilities (MojoSideScheme a) =
    [windowAbility a 1 (W.VillainWouldTakeDamage AnyVillain AnyDamage) ForcedInterrupt NoCost (runAbility a 1) | sideSchemeThreat a > 0]

instance RunMessage MojoSideScheme where
  runMessage msg e@(MojoSideScheme a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(vid, damageAmount d) | W.DamagedVillain vid d <- windows] $ \(vid,n) ->
        insteadOf (\case VillainMessage other (VillainDamaged _ _) -> vid == other; _ -> False) $
          sideSchemeStep (sideSchemeId a) (SideSchemeThreatRemoved (toSource a) n)
      pure e
    _ -> MojoSideScheme <$> runMessage msg a
