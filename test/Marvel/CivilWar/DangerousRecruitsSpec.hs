module Marvel.CivilWar.DangerousRecruitsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Message.Lifted qualified as L
import TestImport

spec :: Spec
spec = describe "Civil War Dangerous Recruits" do
  it "Bullseye discards a controlled upgrade in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      shooter <- putInHand you "01008"
      put <- L.runQueueT $ L.capture $ L.putIntoPlay you shooter
      resolve put
      _ <- addMinion you C.cw56088
      getsGame (HashMap.member (upgradeIdOf shooter) . gameUpgrades) `shouldReturn` False

  it "the opposing team chooses an attachment on the revealing player's leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      leader <- addSecondBoard tg
      decoy <- encounterCard C.cw56084
      attach <- L.runQueueT $ L.capture $ L.attachEncounterCard decoy leader
      resolve attach
      card <- encounterCard C.cw56088
      start [RevealEncounterCard you card]
      fmap fst <$> asked `shouldReturn` Just (player2 tg)
      choose "discard the leader's decoy" (isTargetChoice $ attachmentIdOf decoy)
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Nothing

  it "Bullseye uses otherwise when the friendly leader has only a permanent attachment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      leader <- addSecondBoard tg
      finesse <- encounterCard C.cw56174
      attach <- L.runQueueT $ L.capture $ L.attachEncounterCard finesse leader
      resolve attach
      shooter <- putInHand you "01008"
      put <- L.runQueueT $ L.capture $ L.putIntoPlay you shooter
      resolve put
      _ <- addMinion you C.cw56088
      getAttachedCharacter (attachmentIdOf finesse) `shouldReturn` Just (VillainCharacter leader)
      getsGame (HashMap.member (upgradeIdOf shooter) . gameUpgrades) `shouldReturn` False
