module Marvel.Ally.Allies.ShangChi (
  shangChi,
  ShangChi (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Resource
import Marvel.Window

shangChi :: AllyCard ShangChi
shangChi = ally ShangChi Cards.shangChi (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype ShangChi = ShangChi (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | A Captive ally: "Response: After you play Shang-Chi from your hand, spend a [Energy]
-- resource → stun an enemy."
instance HasAbilities ShangChi where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis (ResourceCost $ Just Energy) $
        runAbility a 1
    ]

instance RunMessage ShangChi where
  runMessage msg a@(ShangChi attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      chooseOne ident [targeting enemy (stun enemy a) | enemy <- enemies]
      pure a
    _ -> ShangChi <$> runMessage msg attrs
