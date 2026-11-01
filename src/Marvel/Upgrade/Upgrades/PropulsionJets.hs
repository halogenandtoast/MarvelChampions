module Marvel.Upgrade.Upgrades.PropulsionJets where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Identity (ironheartVersion)
import Marvel.Matchers
import Marvel.Question (runAbility)

propulsionJets :: UpgradeCard PropulsionJets
propulsionJets = upgrade PropulsionJets Cards.propulsionJets

newtype PropulsionJets = PropulsionJets (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PropulsionJets where
  getAbilities a = [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor PropulsionJets where
  getModifiersFor _ (IdentityRef you) (PropulsionJets a) = pure [HitPointModifier 2 | you == upgradeController a]
  getModifiersFor _ _ _ = pure []

instance RunMessage PropulsionJets where
  runMessage msg e@(PropulsionJets attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      version <- ironheartVersion you
      removeThreat you attrs version ThwartableScheme
      pure e
    _ -> PropulsionJets <$> runMessage msg attrs
