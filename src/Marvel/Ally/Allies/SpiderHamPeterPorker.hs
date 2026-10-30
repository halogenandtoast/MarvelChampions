module Marvel.Ally.Allies.SpiderHamPeterPorker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

spiderHamPeterPorker :: AllyCard SpiderHamPeterPorker
spiderHamPeterPorker = ally SpiderHamPeterPorker Cards.spiderHamPeterPorker (Thw 2, 0) (Atk 2, 0) (HP 3)

newtype SpiderHamPeterPorker = SpiderHamPeterPorker (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderHamPeterPorker where
  getAbilities a@(SpiderHamPeterPorker attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy,
                        W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme])
        ForcedResponse OwnsThis NoCost (runAbility a 1) ]

instance RunMessage SpiderHamPeterPorker where
  runMessage msg a@(SpiderHamPeterPorker attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs ())
      pure a
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> n) -> do
      when (n > 0) $ damageCharacter (allyId attrs) attrs (toDamage n FromAbility)
      pure a
    _ -> SpiderHamPeterPorker <$> runMessage msg attrs
