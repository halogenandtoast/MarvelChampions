module Marvel.Minion.Minions.HydraExoSoldier (
  hydraExoSoldier,
  HydraExoSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraExoSoldier :: MinionCard HydraExoSoldier
hydraExoSoldier = minion HydraExoSoldier Cards.hydraExoSoldier (Sch 2) (Atk 2) (HP 5)

newtype HydraExoSoldier = HydraExoSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HydraExoSoldier where
  runMessage msg e@(HydraExoSoldier attrs) = runQueueT $ case msg of
    -- Boost: give the villain a tough status card and another boost card (one for the
    -- activation under way, which only the villain's is)
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure e
        Just villain -> do
          enemyBecomesTough villain
          when (enemy == EnemyVillainId villain) $ dealBoost villain
          pure e
    _ -> HydraExoSoldier <$> runMessage msg attrs
