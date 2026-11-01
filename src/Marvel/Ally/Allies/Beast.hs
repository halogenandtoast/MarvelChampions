module Marvel.Ally.Allies.Beast where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Entity ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Stats ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window qualified as W

beast :: AllyCard Beast
beast = ally Beast Cards.beast (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Beast = Beast (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Beast where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Beast where
  runMessage msg a@(Beast attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithType ResourceType) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Beast <$> runMessage msg attrs
