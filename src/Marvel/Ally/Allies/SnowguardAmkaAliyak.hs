module Marvel.Ally.Allies.SnowguardAmkaAliyak where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Window qualified as W
import Marvel.Keyword
import Marvel.Trait

snowguardAmkaAliyak :: AllyCard SnowguardAmkaAliyak
snowguardAmkaAliyak = ally SnowguardAmkaAliyak Cards.snowguardAmkaAliyak (Thw 0, 1) (Atk 0, 1) (HP 3)

newtype SnowguardAmkaAliyak = SnowguardAmkaAliyak (Attrs Ally)
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SnowguardAmkaAliyak where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance HasModifiersFor SnowguardAmkaAliyak where
  getModifiersFor _ target (SnowguardAmkaAliyak attrs) | target == toTarget attrs =
    pure $ case allyCounters attrs of
      1 -> [AttackModifier 3, KeywordModifier Overkill]
      2 -> [ThwartModifier 3, TraitModifier Aerial]
      3 -> [HitPointModifier 5, KeywordModifier $ Retaliate 1]
      _ -> []
  getModifiersFor _ _ _ = pure []

instance RunMessage SnowguardAmkaAliyak where
  runMessage msg a@(SnowguardAmkaAliyak attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you [labeled (countLabel "placeShiftCounters" n) (placeCounters attrs n) | n <- [0..3]]
      pure a
    _ -> SnowguardAmkaAliyak <$> runMessage msg attrs
