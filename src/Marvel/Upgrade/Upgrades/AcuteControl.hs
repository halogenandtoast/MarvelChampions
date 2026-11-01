module Marvel.Upgrade.Upgrades.AcuteControl where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

acuteControl :: UpgradeCard AcuteControl
acuteControl = upgrade AcuteControl Cards.acuteControl

newtype AcuteControl = AcuteControl (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AcuteControl where
  getAbilities (AcuteControl a) =
    [limitedWindowAbility a 1 (W.IdentityIgnoresKeyword W.After (IdentityWithId $ upgradeController a) AnyMinion)
      HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage AcuteControl where
  runMessage msg e@(AcuteControl attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let minions = nub [mid | W.IgnoredKeyword who mid <- windows, who == you]
      chooseOne you [targeting (MinionRef mid) (dealDamage mid attrs $ toDamage 2 FromAbility) | mid <- minions]
      pure e
    _ -> AcuteControl <$> runMessage msg attrs
