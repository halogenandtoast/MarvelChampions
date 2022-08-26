module Marvel.Upgrade.Upgrades.SpiderTracer where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message hiding (MinionDefeated)
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

spiderTracer :: UpgradeCard SpiderTracer
spiderTracer = upgrade SpiderTracer Cards.spiderTracer

newtype SpiderTracer = SpiderTracer (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities SpiderTracer where
  getAbilities (SpiderTracer attrs) = case upgradeAttachedEnemy attrs of
    Just (EnemyMinionId minionId) ->
      [ windowAbility
            attrs
            1
            (MinionDefeated When $ MinionWithId minionId)
            ForcedInterrupt
            NoCost
          $ RemoveThreat (toSource attrs) 3 ThwartableScheme
      ]
    _ -> []

instance RunMessage SpiderTracer where
  runMessage msg u@(SpiderTracer a) = case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      PlayedUpgrade -> do
        minions <- selectList AnyMinion
        chooseOne (upgradeController a) $ map
          (\minionId -> TargetLabel
            (MinionTarget minionId)
            [ Run
                [ UpgradeMessage (upgradeId a)
                  $ UpgradeAttachedToEnemy
                  $ EnemyMinionId minionId
                ]
            ]
          )
          minions
        pure u
      _ -> SpiderTracer <$> runMessage msg a
    _ -> SpiderTracer <$> runMessage msg a
