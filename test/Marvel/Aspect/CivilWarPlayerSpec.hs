module Marvel.Aspect.CivilWarPlayerSpec (spec) where

import Marvel.CampaignPlayerTest
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted qualified as L
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Civil War player cards" do
  it "Air Cover finds an attachable Tactic upgrade and spends one fuel" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      cover <- putIntoPlay p "56012"
      tactic <- putInDiscard p "56014"
      _ <- revealNow p Minions.armoredGuard
      choose "Air Cover response" (cardAbility cover)
      choose "Suppressing Fire" (isTargetChoice tactic)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "56014")
      covers <- supportsWith "56012"
      map supportUses covers `shouldBe` [1]

  it "Excelsior spends energy after defending to deal two damage" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      sword <- putIntoPlay p "56045"
      energy <- putInHand p "01088"
      toVillainAttack p
      choose "defend" isDefend
      resolve []
      choose "Excelsior response" (cardAbility sword)
      payWith [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Aggressive Conditioning grants three hit points and one hero ATK" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "56013"
      payment <- traverse (putInHand p) ["01088","01090"]
      playWith card payment
      pass
      hitPointsOf p `shouldReturn` 13
      heroPowers p `shouldReturn` (1,3,3)

  it "Defensive Conditioning grants three hit points and one hero DEF" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "56046" ["01088","01090"]
      pass
      hitPointsOf p `shouldReturn` 16
      heroPowers p `shouldReturn` (1,1,2)

  it "Yellowjacket cannot be played without the Avenger trait" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      clearHand p
      jacket <- putInHand p "56019"
      _ <- putInHand p "01088"
      _ <- putInHand p "01090"
      offers (isPlay jacket) `shouldReturn` False

  it "Yellowjacket takes two consequential damage after attacking" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "56019"
      jacket <- allyIn "56019"
      choose "Yellowjacket attacks" (isAllyAttack jacket)
      chooseVillain
      pass
      (allyDamage <$> allyIn "56019") `shouldReturn` 2

  it "Savage Strike adds six ATK and piercing for one basic attack" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      strike <- putInHand p "56017"
      payment <- traverse (putInHand p) ["01088","01090"]
      choose "basic attack" (basicAttack p)
      choose "Savage Strike" (isPlay strike)
      payWith payment
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8
      heroPowers p `shouldReturn` (1,2,3)

  it "Coup de Grace requires a non-Elite upgraded minion" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.hydraBomber
      coup <- putInHand p "56016"
      _ <- putInHand p "01088"
      offers (isPlay coup) `shouldReturn` False

  it "Hidden Base enters with three stronghold counters" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- paying p "56043" ["01088"]
      pass
      bases <- supportsWith "56043"
      map supportUses bases `shouldBe` [3]

  it "the expansion resource reprints generate their original two icons" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "01087"
      energy <- putInHand p "56020"
      playWith event [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Two-Gun Kid resolves one basic attack against two enemies" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      first <- addMinion p Minions.hydraSoldier
      second <- addMinion p Minions.hydraSoldier
      kid <- putIntoPlay p "56010"
      attrs <- allyIn "56010"
      choose "Two-Gun Kid attacks" (isAllyAttack attrs)
      choose "initial target" (isTargetChoice first)
      choose "Two-Gun Kid interrupt" (cardAbility kid)
      choose "first target" (isTargetChoice first)
      choose "second target" (isTargetChoice second)
      choose "resolve first target" (isTargetChoice first)
      pass
      choose "resolve second target" (isTargetChoice second)
      pass
      minionDamageOf first `shouldReturn` Just 2
      minionDamageOf second `shouldReturn` Just 2
      (allyDamage <$> allyIn "56010") `shouldReturn` 1

  it "Iron Lad adds another Avenger's matching power for one use" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      lad <- putIntoPlay p "56052"
      fury <- putIntoPlay p "01068"
      attrs <- allyIn "56052"
      choose "Iron Lad attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Iron Lad interrupt" (cardAbility lad)
      choose "exhaust the assisting Avenger" (isTargetChoice $ allyIdOf fury)
      pass
      villainDamageNow `shouldReturn` 3
      (allyExhausted <$> allyIn "01068") `shouldReturn` True

  it "Patriot gets tough when his controller engages a minion" do
    tg <- testGame ["hulkling","spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      patriot <- putIntoPlay p "56041"
      mid <- addMinion (player2 tg) Minions.hydraBomber
      L.runQueueT (L.capture $ L.engageMinion mid p) >>= runNow
      choose "Patriot response" (cardAbility patriot)
      pass
      characterHasCondition ToughStatus (AllyCharacter $ allyIdOf patriot) `shouldReturn` True

  it "I Can Do This All Day applies basic DEF without exhausting the hero" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "56047"
      payment <- putInHand p "01071"
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      playWith card [payment]
      pass
      damageOf p `shouldReturn` 1
      isExhaustedNow p `shouldReturn` False

  it "Cuts Both Ways grants retaliate for the phase without applying basic DEF" do
    tg <- testGame ["hulkling"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "56050"
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksWithoutBoost p]
      playWith card []
      pass
      damageOf p `shouldReturn` 2
      villainDamageNow `shouldReturn` 1

  it "Two-Gun Kid's interrupt is unavailable with fewer than two legal enemies" do
    tg <- testGame ["tigra"]
    inGame tg do
      let p = player1 tg
      asHero p
      kid <- putIntoPlay p "56010"
      attrs <- allyIn "56010"
      choose "Two-Gun Kid attacks" (isAllyAttack attrs)
      chooseVillain
      offers (cardAbility kid) `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 2
