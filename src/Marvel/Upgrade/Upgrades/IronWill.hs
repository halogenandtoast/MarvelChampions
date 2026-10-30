module Marvel.Upgrade.Upgrades.IronWill where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

ironWill :: UpgradeCard IronWill
ironWill = upgrade IronWill Cards.ironWill

newtype IronWill = IronWill (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IronWill where
  getAbilities (IronWill a) =
    [limitedWindowAbility a 1 (W.IdentityDiscardsTough W.After $ IdentityWithId $ upgradeController a)
      Response OwnsThis NoCost (runAbility a 1)]

instance HasModifiersFor IronWill where
  getModifiersFor _ (IdentityRef you) (IronWill a) = pure [ThwartModifier 1 | you == upgradeController a]
  getModifiersFor _ _ _ = pure []

instance RunMessage IronWill where
  runMessage msg e@(IronWill attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ drawCards you 1
    _ -> IronWill <$> runMessage msg attrs
