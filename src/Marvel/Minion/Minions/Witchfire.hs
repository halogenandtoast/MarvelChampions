module Marvel.Minion.Minions.Witchfire where

import Marvel.Prelude
import Marvel.Query
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Window qualified as W

witchfire :: MinionCard Witchfire
witchfire = minion Witchfire Cards.witchfire (Sch 1) (Atk 3) (HP 4)

newtype Witchfire = Witchfire (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Witchfire where
  getAbilities a@(Witchfire attrs) = [limitedWindowAbility a 1
    (W.AllyDefeatedByAttackOf W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage Witchfire where
  runMessage msg a@(Witchfire attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      ruler <- selectOne (SideSchemeIs Schemes.rulerOfLimbo)
      case ruler of
        Just scheme -> placeThreatOn attrs scheme 1
        Nothing -> placeThreatOnEach attrs 1 MainScheme
      pure a
    _ -> Witchfire <$> runMessage msg attrs
