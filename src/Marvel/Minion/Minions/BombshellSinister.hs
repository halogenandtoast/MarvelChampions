module Marvel.Minion.Minions.BombshellSinister where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner
import Marvel.Game.Source

bombshellSinister :: MinionCard BombshellSinister
bombshellSinister = minion BombshellSinister Cards.bombshellSinister (Sch 2) (Atk 3) (HP 4)

newtype BombshellSinister = BombshellSinister (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BombshellSinister where
  runMessage msg e@(BombshellSinister attrs) = runQueueT $ case msg of
    MinionMessage mid (MinionBeginAttack _) | mid == minionId attrs -> do
      makeAttackDividedEvenly mid
      attrs' <- runMessage msg attrs
      pure $ BombshellSinister attrs'
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ (\you -> dealIndirectDamageAndExhaustDamaged you attrs 1) =<< getPlayers
      pure e
    _ -> BombshellSinister <$> runMessage msg attrs
