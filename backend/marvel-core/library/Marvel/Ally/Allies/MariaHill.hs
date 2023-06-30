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
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $
        runAbility a 1
    ]

newtype MariaHill = MariaHill (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage MariaHill where
  runMessage msg a@(MariaHill attrs) = case msg of
    RanAbility (isTarget a -> True) 1 _ _ -> do
      players <- getPlayers
      pushAll [IdentityMessage p $ DrawCards FromDeck 1 | p <- players]
      pure a
    _ -> MariaHill <$> runMessage msg attrs
