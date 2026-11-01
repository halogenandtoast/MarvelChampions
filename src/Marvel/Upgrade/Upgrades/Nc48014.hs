module Marvel.Upgrade.Upgrades.Nc48014 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

nc48014 :: UpgradeCard Nc48014
nc48014 = upgrade Nc48014 Cards.nc48014

newtype Nc48014 = Nc48014 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nc48014 where
  getAbilities a = [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy $ DamageByPlayer You)
    Response (OwnsThis <> DuringPhase VillainPhase) ExhaustCost (runAbility a 1)]

instance RunMessage Nc48014 where
  runMessage msg a@(Nc48014 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 2
    _ -> Nc48014 <$> runMessage msg attrs
