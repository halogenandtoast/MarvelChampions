module Marvel.Ally.Allies.MoondragonHeatherDouglas (
  moondragonHeatherDouglas,
  MoondragonHeatherDouglas (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query

moondragonHeatherDouglas :: AllyCard MoondragonHeatherDouglas
moondragonHeatherDouglas = ally MoondragonHeatherDouglas Cards.moondragonHeatherDouglas (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype MoondragonHeatherDouglas = MoondragonHeatherDouglas (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action: Exhaust and discard Moondragon → choose a minion. That minion attacks another
-- enemy of your choice. Any minion, and the other enemy may be the villain (ruling).
instance HasAbilities MoondragonHeatherDouglas where
  getAbilities a =
    [ability a 1 Action (OwnsThis <> MinionExists AnyMinion) (ExhaustCost <> DiscardCost (toTarget a)) $ runAbility a 1]

instance RunMessage MoondragonHeatherDouglas where
  runMessage msg a@(MoondragonHeatherDouglas attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      minions <- selectList AnyMinion
      enemies <- selectList AnyEnemy
      chooseOne you
        [ targeting (enemyRef minion) $
          chooseOne you
            [ targeting (enemyRef enemy) (minionAttacksEnemy minion enemy)
            | enemy <- enemies
            , enemy /= toEnemy minion
            ]
        | minion <- minions
        ]
      pure a
    _ -> MoondragonHeatherDouglas <$> runMessage msg attrs
