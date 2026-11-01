module Marvel.Ally.Allies.AngelPsylocke where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Payment
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

angelPsylocke :: AllyCard AngelPsylocke
angelPsylocke = ally AngelPsylocke Cards.angelPsylocke (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype AngelPsylocke = AngelPsylocke (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AngelPsylocke where
  getAbilities (AngelPsylocke a) = [limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage AngelPsylocke where
  runMessage msg e@(AngelPsylocke a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ ready you
    _ -> AngelPsylocke <$> runMessage msg a
