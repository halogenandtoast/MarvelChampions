module Marvel.Ally.Allies.BobAgentOfHydra where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

bobAgentOfHydra :: AllyCard BobAgentOfHydra
bobAgentOfHydra = ally BobAgentOfHydra Cards.bobAgentOfHydra (Thw 1, 0) (Atk 1, 0) (HP 2)

newtype BobAgentOfHydra = BobAgentOfHydra (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BobAgentOfHydra where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage BobAgentOfHydra where
  runMessage msg e@(BobAgentOfHydra attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      schemes <- selectList ThwartableScheme
      chooseOne you $
        [labeled (countLabel "dealDamageToAnEnemy" 2) (chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy) | notNull enemies]
        <> [labeled (countLabel "removeThreatFromAScheme" 1) (removeThreat you attrs 1 ThwartableScheme) | notNull schemes]
      pure e
    _ -> BobAgentOfHydra <$> runMessage msg attrs
