module Marvel.Upgrade.Upgrades.Gatekeeper where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Keyword

gatekeeper :: UpgradeCard Gatekeeper
gatekeeper = upgrade Gatekeeper Cards.gatekeeper

newtype Gatekeeper = Gatekeeper (Attrs Upgrade)

  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Gatekeeper where
  getAbilities (Gatekeeper a) =
    [(windowAbility a 1 (W.MinionDefeated W.When $ maybe AnyMinion MinionWithId ((\case Just (EnemyMinionId mid) -> Just mid; _ -> Nothing) (upgradeAttachedEnemy a))) Interrupt NoCost (runAbility a 1)) {abilityCriteria = SchemeExists (MainScheme <> SchemeWithThreat (GreaterThan $ Static 0))}]

instance HasModifiersFor Gatekeeper where
  getModifiersFor _ (MinionRef mid) (Gatekeeper a) = pure $ if Just mid == (\case Just (EnemyMinionId mid) -> Just mid; _ -> Nothing) (upgradeAttachedEnemy a) then [HitPointModifier 2, KeywordModifier Patrol] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Gatekeeper where
  runMessage msg e@(Gatekeeper attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      removeThreat you attrs 4 MainScheme
      pure e
    _ -> Gatekeeper <$> runMessage msg attrs

instance IsUpgrade Gatekeeper where
  upgradeTargets _ = Just . map toTarget <$> selectList AnyMinion
