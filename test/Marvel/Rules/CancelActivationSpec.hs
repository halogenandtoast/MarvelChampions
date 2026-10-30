module Marvel.Rules.CancelActivationSpec (spec) where

import Marvel.Message.Lifted qualified as Lifted
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "cancelling an enemy activation" do
  it "cancels this attack without defending or cancelling the next activation" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      let enemy = EnemyVillainId vid
          ending = CheckWindows [W.Window W.After $ W.EnemyActivation enemy p]
          cleanup = VillainMessage vid VillainEndAttack
          later = VillainMessage vid (VillainBeginAttack p)
      withQueue_ $ const
        [ CheckWindows [W.Window W.Would $ W.EnemyAttack enemy p]
        , VillainMessage vid (VillainBeginAttack p)
        , cleanup, ending, later ]
      Lifted.cancelEnemyActivation enemy
      withQueueM pure `shouldReturn` [cleanup, later]

  it "cancels a scheme's boosts, threat and completion response" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      let enemy = EnemyVillainId vid
          ending = CheckWindows [W.Window W.After $ W.EnemyActivation enemy p]
      withQueue_ $ const
        [ DealBoost (VillainRef vid)
        , VillainMessage vid VillainFlipBoostCards
        , VillainMessage vid VillainSchemed
        , ClearBoosts, ending ]
      Lifted.cancelEnemyActivation enemy
      withQueueM pure `shouldReturn` [ClearBoosts]

  it "cancels overkill and both damage windows when the minion's defeat is replaced" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      mid <- getRandom
      let damage = toDamage 4 FromOverkill
      withQueue_ $ const
        [ MinionMessage mid MinionDefeated
        , CheckWindows [W.Window W.When $ W.DamagedVillain vid damage, W.Window W.When $ W.VillainDamageFrom vid GameRef damage]
        , VillainMessage vid (VillainDamaged GameRef damage)
        , CheckWindows [W.Window W.After $ W.DefeatedMinion mid damage GameRef 4]
        ]
      cancelMinionDefeat mid
      withQueueM pure `shouldReturn` []
