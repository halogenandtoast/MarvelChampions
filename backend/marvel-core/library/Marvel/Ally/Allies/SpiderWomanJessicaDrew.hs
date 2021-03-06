module Marvel.Ally.Allies.SpiderWomanJessicaDrew (
  spiderWomanJessicaDrew,
  SpiderWomanJessicaDrew (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
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

newtype SpiderWomanJessicaDrew = SpiderWomanJessicaDrew AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities SpiderWomanJessicaDrew where
  getAbilities a = [limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $ runAbility a 1]

instance RunMessage SpiderWomanJessicaDrew where
  runMessage msg a = case msg of
    RanAbility (isTarget a -> True) 1 _ -> do
      villain <- selectJust ActiveVillain
      push $ VillainMessage villain $ VillainConfused (toSource a)
      pure a
    _ -> SpiderWomanJessicaDrew <$> runMessage msg (toAttrs a)
