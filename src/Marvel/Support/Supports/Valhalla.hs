module Marvel.Support.Supports.Valhalla (valhalla, Valhalla (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

valhalla :: SupportCard Valhalla
valhalla = support Valhalla Cards.valhalla

newtype Valhalla = Valhalla (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Valhalla where
  getAbilities a@(Valhalla attrs) =
    [ limitedWindowAbility a 1
        (W.EnemyDefeatedWithUpgrade W.After Upgrades.deathGlow $ AttackFromPlayer $ IdentityWithId $ supportController attrs)
        Response OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage Valhalla where
  runMessage msg s@(Valhalla attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      drawCards ident 1
      heal ident 1
      pure s
    _ -> Valhalla <$> runMessage msg attrs
