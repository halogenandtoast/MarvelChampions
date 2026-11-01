module Marvel.Upgrade.Upgrades.IntangibleInterference where

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
import Marvel.Damage ()
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

intangibleInterference :: UpgradeCard IntangibleInterference
intangibleInterference = upgrade IntangibleInterference Cards.intangibleInterference

newtype IntangibleInterference = IntangibleInterference (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IntangibleInterference where
  getAbilities (IntangibleInterference a) =
    [limitedWindowAbility a 1 (W.IdentityIgnoresCrisis W.After (IdentityWithId $ upgradeController a) AnySideScheme)
      HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage IntangibleInterference where
  runMessage msg e@(IntangibleInterference attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let schemes = nub [sid | W.IgnoredCrisis who sid <- windows, who == you]
      chooseOne you [targeting (SideSchemeRef sid) (thwartScheme sid attrs 2) | sid <- schemes]
      pure e
    _ -> IntangibleInterference <$> runMessage msg attrs
