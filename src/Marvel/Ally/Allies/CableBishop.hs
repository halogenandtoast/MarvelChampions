module Marvel.Ally.Allies.CableBishop where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

cableBishop :: AllyCard CableBishop
cableBishop = ally CableBishop Cards.cableBishop (Thw 2, 1) (Atk 3, 2) (HP 3)

newtype CableBishop = CableBishop (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CableBishop where
  getAbilities a@(CableBishop attrs) =
    [limitedWindowAbility a 1 (W.SideSchemeDefeatedByThwart AnySideScheme $ SourceIs $ toSource attrs)
      Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage CableBishop where
  runMessage msg a@(CableBishop attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> CableBishop <$> runMessage msg attrs
