module Marvel.CivilWar.MartialLawSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import TestImport

spec :: Spec
spec = describe "Civil War Martial Law" do
  it "S.H.I.E.L.D. Soldier puts only the topmost matching minion into play without its reveal effect" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (_,discard) <- setEncounterDeck [M.hydraMercenary] [M.hydraSoldier,C.cw56109,C.cw56083]
      soldier <- encounterCard C.cw56109
      resolve [RevealEncounterCard you soldier]
      let mid = MinionId $ unCardId $ ecCardId soldier
      minions <- getsGame gameMinions
      HashMap.size minions `shouldBe` 2
      HashMap.member mid minions `shouldBe` True
      let revived = MinionId $ unCardId $ ecCardId $ discard !! 1
      HashMap.member revived minions `shouldBe` True
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode M.hydraSoldier,"56083"]

  it "S.H.I.E.L.D. Soldier has no extra effect without a matching discarded minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [M.hydraMercenary] [M.hydraSoldier,C.cw56085]
      _ <- addMinion (player1 tg) C.cw56109
      getsGame (HashMap.size . gameMinions) `shouldReturn` 1
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode M.hydraSoldier,"56085"]

  it "S.H.I.E.L.D. Soldier's boost enters play without reviving another minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      (soldier : _,_) <- setEncounterDeck [C.cw56109,M.hydraMercenary] [C.cw56083]
      resolve [VillainMessage vid VillainSchemes]
      let mid = MinionId $ unCardId $ ecCardId soldier
      minionDamageOf mid `shouldReturn` Just 0
      getsGame (HashMap.size . gameMinions) `shouldReturn` 1
      map toCardCode <$> getEncounterDiscard `shouldReturn` ["56083"]

  it "S.H.I.E.L.D. Soldier searches only its revealing player's encounter discard" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      ownLeader <- villainIdNow
      otherLeader <- addSecondBoard tg
      let ownKey = L.VillainDeckKey ownLeader
          otherKey = L.VillainDeckKey otherLeader
      foreignCard <- encounterCard C.cw56083
      filler <- encounterCard M.hydraMercenary
      piles <- L.runQueueT $ L.capture $ do
        L.createDeck ownKey []
        L.createDeck otherKey [foreignCard,filler]
        L.discardCard foreignCard
      resolve piles
      before <- getsGame (HashMap.size . gameMinions)
      _ <- addMinion p C.cw56109
      getsGame (HashMap.size . gameMinions) `shouldReturn` (before + 1)
      asYou q getEncounterDiscard `shouldReturn` [foreignCard]

  for_ [False,True] $ \hero ->
    it ("Rapid Response discards through a minion and uses its printed stats: hero " <> show hero) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        (deck,_) <- setEncounterDeck [C.cw56089,M.hydraSoldier,M.hydraMercenary] []
        card <- encounterCard C.cw56110
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` if hero then 2 else 0
        mainSchemeThreat `shouldReturn` if hero then 0 else 1
        map toCardCode <$> getEncounterDiscard `shouldReturn` ["56110",toCardCode M.hydraSoldier,"56089"]
        getEncounterDeck `shouldReturn` drop 2 deck
        getsGame (HashMap.size . gameMinions) `shouldReturn` 0

  it "Rapid Response's hero damage is prevented by tough and does not consume stun" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      resolve [IdentityMessage you IdentityBecomeTough,GiveStatus (IdentityRef you) StunnedStatus]
      _ <- setEncounterDeck [M.hydraSoldier,M.hydraMercenary] []
      card <- encounterCard C.cw56110
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 0
      playerIdentityTough <$> playerAttrs you `shouldReturn` False
      playerIdentityStunned <$> playerAttrs you `shouldReturn` True

  it "Rapid Response does nothing further if no minion is discarded" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [C.cw56089,C.cw56085] []
      card <- encounterCard C.cw56110
      resolve [RevealEncounterCard (player1 tg) card]
      damageOf (player1 tg) `shouldReturn` 0
      mainSchemeThreat `shouldReturn` 0

  it "S.H.I.E.L.D. Patrol starts with hinder threat and an amplify icon" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      side <- encounterCard C.cw56111
      resolve [RevealEncounterCard (player1 tg) side]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 5
      cdAmplify (getCardDef side) `shouldBe` 1

  it "S.H.I.E.L.D. Patrol grants guard to current and later minions until it leaves play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      first <- addMinion you M.shocker
      side <- encounterCard C.cw56111
      resolve [RevealEncounterCard you side]
      second <- addMinion you C.cw56109
      for_ [first,second] $ \mid -> getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Guard)
      resolve [RemoveFromPlay $ SideSchemeRef $ SideSchemeId $ unCardId $ ecCardId side]
      for_ [first,second] $ \mid -> getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` notElem K.Guard)

  it "S.H.I.E.L.D. Patrol grants guard only to minions on its own board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      _ <- addSecondBoard tg
      own <- addMinion p M.shocker
      other <- addMinion q M.shocker
      side <- encounterCard C.cw56111
      resolve [RevealEncounterCard p side]
      getEnemyKeywords (EnemyMinionId own) >>= (`shouldSatisfy` elem K.Guard)
      getEnemyKeywords (EnemyMinionId other) >>= (`shouldSatisfy` notElem K.Guard)


  it "Rapid Response does not apply in-play minion stat modifiers to the discarded card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      modified <- L.runQueueT $ L.capture $
        L.lastingModifiers GameRef (MinionEntity AnyMinion) [AttackModifier 7,SchemeModifier 7] False
      resolve modified
      _ <- setEncounterDeck [M.hydraSoldier,M.hydraMercenary] []
      card <- encounterCard C.cw56110
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 2

  it "blanking S.H.I.E.L.D. Patrol turns off its guard modifier" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you M.shocker
      side <- encounterCard C.cw56111
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      blank <- L.runQueueT $ L.capture $
        L.lastingModifiers GameRef (SourceEntity $ SideSchemeRef sid) [Blank] False
      resolve blank
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` notElem K.Guard)
