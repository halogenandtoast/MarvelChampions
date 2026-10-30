module Marvel.Minion.Minions.HobgoblinSinister where

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
import Marvel.Window qualified as W

hobgoblinSinister :: MinionCard HobgoblinSinister
hobgoblinSinister = minion HobgoblinSinister Cards.hobgoblinSinister (Sch 2) (Atk 2) (HP 5)

newtype HobgoblinSinister = HobgoblinSinister (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HobgoblinSinister where
  getAbilities a@(HobgoblinSinister attrs) =
    [ windowAbility a 1 (W.EnemyWouldAttack (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
        ForcedInterrupt NoCost (runAbility a 1)
    ]

instance RunMessage HobgoblinSinister where
  runMessage msg e@(HobgoblinSinister attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      attack <- getModifiedAttack attrs
      for_ [you | W.EnemyAttack _ you <- windows] \you ->
        insteadOf (\case
          MinionMessage mid (MinionBeginAttack _) -> mid == minionId attrs
          _ -> False) $
          discardTopOfEncounterDeckThen attack (countBoostIcons attrs you)
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n > 0) $ dealIndirectDamage you attrs n
      pure e
    _ -> HobgoblinSinister <$> runMessage msg attrs
