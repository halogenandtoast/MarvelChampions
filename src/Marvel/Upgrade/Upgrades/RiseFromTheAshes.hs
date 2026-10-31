module Marvel.Upgrade.Upgrades.RiseFromTheAshes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

riseFromTheAshes :: UpgradeCard RiseFromTheAshes
riseFromTheAshes = upgrade RiseFromTheAshes Cards.riseFromTheAshes

newtype RiseFromTheAshes = RiseFromTheAshes (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RiseFromTheAshes where
  getAbilities (RiseFromTheAshes a) =
    [limitedWindowAbility a 1 (W.IdentityWouldBeDefeated $ IdentityWithId $ upgradeController a) Interrupt OwnsThis (RemoveFromGameCost $ toRef a) (runAbility a 1)]

instance RunMessage RiseFromTheAshes where
  runMessage msg e@(RiseFromTheAshes a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      cancelMatchingMessage (== IdentityMessage you IdentityDefeated)
      ready you
      setHitPoints you 9
      removeAllPhoenixPower you
      pure e
    _ -> RiseFromTheAshes <$> runMessage msg a
