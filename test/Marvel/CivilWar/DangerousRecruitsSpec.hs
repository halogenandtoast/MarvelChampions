module Marvel.CivilWar.DangerousRecruitsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Keyword qualified as K
import Marvel.Matchers
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


  it "Venom schemes against the revealing alter-ego in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreat
      _ <- addMinion (player1 tg) C.cw56087
      mainSchemeThreat `shouldReturn` (before + 1)
      damageOf (player1 tg) `shouldReturn` 0

  it "Venom's cooperative attack pierces the revealing hero's tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- addMinion you C.cw56087
      damageOf you `shouldReturn` 3
      playerIdentityTough <$> playerAttrs you `shouldReturn` False

  it "Venom attacks the friendly leader across boards without asking a player to defend" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      leader <- addSecondBoard tg
      before <- leaderHp leader
      resolve [VillainMessage leader VillainBecomeTough]
      card <- encounterCard C.cw56087
      start [RevealEncounterCard (player1 tg) card]
      leaderHp leader `shouldReturn` (before - 3)
      damageOf (player1 tg) `shouldReturn` 0
      damageOf (player2 tg) `shouldReturn` 0
      getEnemyAttack (EnemyMinionId $ MinionId $ unCardId $ ecCardId card) `shouldReturn` Nothing

  it "the leader retaliates after Venom's cross-board attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      leader <- withLeader "56139"
      _ <- addSecondBoard tg
      shield <- encounterCard C.cw56143
      attach <- L.runQueueT $ L.capture $ L.attachEncounterCard shield leader
      resolve attach
      resolve [RemoveStatus (VillainRef leader) ToughStatus]
      before <- leaderHp leader
      mid <- addMinion (player2 tg) C.cw56087
      leaderHp leader `shouldReturn` (before - 3)
      minionDamageOf mid `shouldReturn` Just 1

  it "Mass Increase prevents Venom's cross-board attack and its prevention expires at attack end" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      leader <- withLeader "57040"
      _ <- addSecondBoard tg
      dense <- encounterCard C.cw57046a
      mass <- encounterCard C.cw57049
      attach <- L.runQueueT $ L.capture $ do
        L.attachEncounterCard dense leader
        L.attachEncounterCard mass leader
      resolve attach
      before <- leaderHp leader
      mid <- addMinion (player2 tg) C.cw56087
      leaderHp leader `shouldReturn` before
      asYou (player2 tg) (enemyMatches StunnedEnemy $ EnemyMinionId mid) `shouldReturn` True
      getAttachedCharacter (attachmentIdOf mass) `shouldReturn` Nothing
      resolve [RemoveStatus (MinionRef mid) StunnedStatus]
      attack <- L.runQueueT $ L.capture $ L.minionAttacksEnemyAs mid (player2 tg) leader
      resolve attack
      leaderHp leader `shouldReturn` (before - 3)


  it "a forbidden minion attack does not leave a stale enemy target" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      mid <- addMinion you M.hydraMercenary
      messages <- L.runQueueT $ L.capture $ do
        L.lastingModifiers mid (SourceEntity $ MinionRef mid) [CannotActivate] False
        L.minionAttacksEnemy mid vid
      resolve messages
      villainDamageNow `shouldReturn` 0
      getsGame (fmap (minionAttackTarget . toAttrs) . lookup mid . gameMinions) `shouldReturn` Just Nothing

  it "a stunned minion spends its status instead of attacking another enemy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      mid <- addMinion you M.hydraMercenary
      resolve [GiveStatus (MinionRef mid) StunnedStatus]
      messages <- L.runQueueT $ L.capture $ L.minionAttacksEnemy mid vid
      resolve messages
      enemyMatches StunnedEnemy (EnemyMinionId mid) `shouldReturn` False
      villainDamageNow `shouldReturn` 0
      getEnemyAttack (EnemyMinionId mid) `shouldReturn` Nothing


  it "a villainous minion uses its own deck and resolves boost 'you' as the nominated player" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      let first = player1 tg
          engaged = seats tg !! 1
          opponent = last (seats tg)
      ownLeader <- villainIdNow
      target <- addSecondBoard tg
      boost <- encounterCard C.cw56078
      other <- encounterCard M.hydraSoldier
      card <- encounterCard C.cw56087
      prepare <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey ownLeader) [boost]
        L.createDeck (L.VillainDeckKey target) [other]
        L.putEncounterCardIntoPlay engaged card
      resolve prepare
      otherBefore <- asYou opponent getEncounterDeck
      let mid = MinionId $ unCardId $ ecCardId card
      before <- leaderHp target
      attack <- L.runQueueT $ L.capture $ do
        L.lastingModifiers mid (SourceEntity $ MinionRef mid) [KeywordModifier K.Villainous] False
        L.minionAttacksEnemyAs mid first target
      resolve attack
      leaderHp target `shouldReturn` (before - 3)
      identityMatches ConfusedIdentity first `shouldReturn` True
      identityMatches ConfusedIdentity engaged `shouldReturn` False
      asYou opponent getEncounterDeck `shouldReturn` otherBefore
      getActivePlayerId `shouldReturn` first
