module Marvel.Upgrade.Upgrades.StingOperationShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Discard ()
import Marvel.Deck ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

stingOperationShuri :: UpgradeCard StingOperationShuri
stingOperationShuri = upgrade StingOperationShuri Cards.stingOperationShuri
newtype StingOperationShuri = StingOperationShuri (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage StingOperationShuri where
  runMessage msg u@(StingOperationShuri attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.SchemedByMinion mid <- windows] removeFromPlay
      pure u
    _ -> StingOperationShuri <$> runMessage msg attrs

instance HasAbilities StingOperationShuri where
  getAbilities u@(StingOperationShuri attrs) = [limitedWindowAbility u 1 (W.MinionHasSchemed W.After (NotMinion $ MinionWithTrait T.Elite)) Response OwnsThis (DiscardCost $ toTarget attrs) (runAbility u 1)]
