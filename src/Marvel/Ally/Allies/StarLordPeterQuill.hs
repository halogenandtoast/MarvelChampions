module Marvel.Ally.Allies.StarLordPeterQuill (
  starLordPeterQuill,
  StarLordPeterQuill (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

starLordPeterQuill :: AllyCard StarLordPeterQuill
starLordPeterQuill = ally StarLordPeterQuill Cards.starLordPeterQuill (Thw 2, 1) (Atk 2, 1) (HP 3)

-- | "[star] Star-Lord's attacks gain ranged": the keyword on his card def.
newtype StarLordPeterQuill = StarLordPeterQuill (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | "Forced Response: After Star-Lord enters play under your control, deal yourself 1
-- facedown encounter card."
instance HasAbilities StarLordPeterQuill where
  getAbilities (StarLordPeterQuill a) =
    [ limitedWindowAbility a 1 W.EntersPlayThis ForcedResponse OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage StarLordPeterQuill where
  runMessage msg s@(StarLordPeterQuill attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget s -> True) 1 _ _ -> do
      dealEncounterCard (allyController attrs)
      pure s
    _ -> StarLordPeterQuill <$> runMessage msg attrs
