module Marvel.Minion.Minions.TheGreenGobbler where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Query ()
import Marvel.Window qualified as W

theGreenGobbler :: MinionCard TheGreenGobbler
theGreenGobbler = minion TheGreenGobbler Cards.theGreenGobbler (Sch 1) (Atk 2) (HP 4)

newtype TheGreenGobbler = TheGreenGobbler (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheGreenGobbler where
  getAbilities (TheGreenGobbler a) =
    [windowAbility a 1 (W.EngagesMinion W.After AnyIdentity $ MinionWithId $ minionId a) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage TheGreenGobbler where
  runMessage msg e@(TheGreenGobbler attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
          self = IdentityWithId you
      allies <- selectList (AllyControlledBy self)
      supports <- selectList (SupportControlledBy self)
      upgrades <- selectList (UpgradeControlledBy self)
      let cards = IdentityRef you : map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades
      for_ cards \card -> getCountersOn card >>= removeCounters card
      pure e
    _ -> TheGreenGobbler <$> runMessage msg attrs
