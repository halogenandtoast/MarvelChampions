module Marvel.Ally.Allies.SpiderManPeterParker (
  spiderManPeterParker,
  SpiderManPeterParker (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

spiderManPeterParker :: AllyCard SpiderManPeterParker
spiderManPeterParker = ally SpiderManPeterParker Cards.spiderManPeterParker (Thw 2, 1) (Atk 2, 1) (HP 4)

newtype SpiderManPeterParker = SpiderManPeterParker (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Response: After you play Spider-Man from your hand, remove 3 [per_hero] threat from
-- a side scheme.
instance HasAbilities SpiderManPeterParker where
  getAbilities a =
    [limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $ runAbility a 1]

instance RunMessage SpiderManPeterParker where
  runMessage msg a@(SpiderManPeterParker attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      schemes <- selectList ThwartableSchemeIgnoringCrisis
      n <- (* 3) . fromIntegral <$> getPlayerCount
      chooseOne ident [targeting (SchemeRef s) (thwartScheme s a n) | s@(SchemeSideSchemeId _) <- schemes]
      pure a
    _ -> SpiderManPeterParker <$> runMessage msg attrs
