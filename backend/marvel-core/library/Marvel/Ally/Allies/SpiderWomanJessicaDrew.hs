module Marvel.Ally.Allies.SpiderWomanJessicaDrew (
  spiderWomanJessicaDrew,
  SpiderWomanJessicaDrew (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

spiderWomanJessicaDrew :: AllyCard SpiderWomanJessicaDrew
spiderWomanJessicaDrew =
  ally
    SpiderWomanJessicaDrew
    Cards.spiderWomanJessicaDrew
    (Thw 2, 1)
    (Atk 2, 1)
    (HP 2)

newtype SpiderWomanJessicaDrew = SpiderWomanJessicaDrew (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderWomanJessicaDrew where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage SpiderWomanJessicaDrew where
  runMessage msg a@(SpiderWomanJessicaDrew attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      villain <- selectJust ActiveVillain
      push $ VillainMessage villain $ VillainConfused (toSource a)
      pure a
    _ -> SpiderWomanJessicaDrew <$> runMessage msg attrs
