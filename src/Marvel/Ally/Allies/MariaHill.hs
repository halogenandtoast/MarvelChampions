module Marvel.Ally.Allies.MariaHill where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Window

mariaHill :: AllyCard MariaHill
mariaHill = ally MariaHill Cards.mariaHill (Thw 2, 1) (Atk 1, 1) (HP 2)

instance HasAbilities MariaHill where
  getAbilities a =
    [ limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $
        runAbility a 1
    ]

newtype MariaHill = MariaHill (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage MariaHill where
  runMessage msg a@(MariaHill attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      players <- getPlayers
      for_ players (`drawCards` 1)
      pure a
    _ -> MariaHill <$> runMessage msg attrs
