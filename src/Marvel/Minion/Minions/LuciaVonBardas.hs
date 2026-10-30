module Marvel.Minion.Minions.LuciaVonBardas where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Game.Source (getModifiers)
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Phase

luciaVonBardas :: MinionCard LuciaVonBardas
luciaVonBardas = minion LuciaVonBardas Cards.luciaVonBardas (Sch 2) (Atk 1) (HP 4)

newtype LuciaVonBardas = LuciaVonBardas (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LuciaVonBardas where
  getModifiersFor _ target (LuciaVonBardas attrs)
    | isTarget attrs target = pure $ if minionTough attrs then [AttackModifier 1, SchemeModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage LuciaVonBardas where
  runMessage msg m@(LuciaVonBardas attrs) = runQueueT $ case msg of
    EndPhase VillainPhase -> do
      blanked <- elem Blank <$> getModifiers attrs
      unless blanked $ enemyBecomesTough (minionId attrs)
      pure m
    _ -> LuciaVonBardas <$> runMessage msg attrs
