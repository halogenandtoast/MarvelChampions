module Marvel.Hero.NickFurySignatureSpec (spec) where
import Marvel.AgentsOfShield.PlayerHelpers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Nick Fury signature cards" do
  it "Concentrated Fire deals four ranged attack damage" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      event <- putInHand p "50037"
      resource <- putInHand p "01088"
      playWith event [resource]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "Concentrated Fire collects printed SCH when it defeats its enemy" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      minion <- addMinion p Minions.hydraBomber
      event <- putInHand p "50037"
      resource <- putInHand p "01088"
      playWith event [resource]
      choose "minion" (isTargetChoice minion)
      pass
      choose "collect threat" (isLabel "$label.addThreat")
      pass
      suitThreat p `shouldReturn` 1

  it "Covert Surveillance removes threat then can change to Stealth" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      placeMainThreat 5
      event <- putInHand p "50038"
      resource <- putInHand p "01088"
      playWith event [resource]
      chooseMainScheme
      choose "Stealth" (isLabel "$label.changeSuit")
      pass
      mainSchemeThreatNow `shouldReturn` 3
      isStealth p `shouldReturn` True

  it "Covert Surveillance in Stealth places only threat actually removed on the suit" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      L.runQueueT (L.capture $ changeSuit p True) >>= runNow
      changeForm p
      pass
      clearHand p
      placeMainThreat 1
      event <- putInHand p "50038"
      resource <- putInHand p "01088"
      playWith event [resource]
      chooseMainScheme
      choose "collect threat" (isLabel "$label.changeSuit")
      pass
      suitThreat p `shouldReturn` 1

  it "Spray Fire damages the villain and only the chosen player's engaged minions" do
    tg <- testGame ["nick-fury", "spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      target <- addMinion p Minions.hydraBomber
      other <- addMinion (player2 tg) Minions.hydraBomber
      clearHand p
      event <- putInHand p "50039"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith event [r1,r2]
      choose "Nick" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 3
      minionDamageOf target `shouldReturn` Nothing
      minionDamageOf other `shouldReturn` Just 0

  it "Flying Car spends one suit threat to ready Nick and grant Aerial" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      car <- putIntoPlay p "50040"
      L.runQueueT (L.capture $ placeSuitThreat p 2) >>= runNow
      runNow [IdentityMessage p ExhaustedIdentity]
      choose "Flying Car" (\c -> isAbilityOfCard car c)
      pass
      exhaustedNow p `shouldReturn` False
      suitThreat p `shouldReturn` 1

  it "Flying Car cannot be used without suit threat" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      car <- putIntoPlay p "50040"
      offers (isAbilityOfCard car) `shouldReturn` False

  it "Safe House heals two without spending suit threat" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      house <- putIntoPlay p "50041"
      setDamage p 4
      choose "Safe House" (isAbilityOfCard house)
      choose "heal" (isLabel "$label.heal")
      pass
      damageOf p `shouldReturn` 2
      suitThreat p `shouldReturn` 0

  it "Eyepatch Camera diverts at most three of an incoming main-scheme threat placement" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      camera <- putIntoPlay p "50043"
      scheme <- mainSchemeIdNow
      before <- mainSchemeThreatNow
      L.runQueueT (L.capture $ L.placeThreatOn GameRef scheme 5) >>= start
      choose "camera" (isAbilityOfCard camera)
      choose "divert three" (isLabel $ L.countLabel "aosMoveThreat" 3)
      pass
      mainSchemeThreatNow `shouldReturn` (before + 2)
      suitThreat p `shouldReturn` 3

  it "Watch spends two suit threat to generate two mental resources" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      watch <- putIntoPlay p "50044"
      L.runQueueT (L.capture $ placeSuitThreat p 3) >>= runNow
      clearHand p
      shield <- putInHand p "50042"
      choose "play shield" (isPlay shield)
      choose "watch for two" (\case UseAbility ab -> isAbilityOfCard watch (UseAbility ab) && abilityIndex ab == 2; _ -> False)
      payWith []
      pass
      suitThreat p `shouldReturn` 1

  it "Maria Hill collects only threat actually removed by her basic thwart" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      maria <- putIntoPlay p "50036"
      ally <- allyIn "50036"
      placeMainThreat 1
      choose "Maria thwarts" (isAllyThwart ally)
      chooseMainScheme
      choose "collect removed threat" (isAbilityOfCard maria)
      pass
      mainSchemeThreatNow `shouldReturn` 0
      suitThreat p `shouldReturn` 1

  it "Intelligence Analysis cancels a treachery in alter ego using one suit threat" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      analysis <- putIntoPlay p "50045"
      L.runQueueT (L.capture $ placeSuitThreat p 1) >>= runNow
      card <- revealNow p Treacheries.falseAlarm
      choose "Intelligence Analysis" (isAbilityOfCard analysis)
      pass
      suitThreat p `shouldReturn` 0
      isConfused p `shouldReturn` False
      encounterDiscard >>= (`shouldSatisfy` elem card)

  it "Intelligence Analysis cannot cancel without suit threat" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      analysis <- putIntoPlay p "50045"
      _ <- revealNow p Treacheries.falseAlarm
      offers (isAbilityOfCard analysis) `shouldReturn` False
      pass

  it "EM Shield prevents all damage from an undefended minion attack" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      shield <- putIntoPlay p "50042"
      mid <- addMinion p Minions.hydraSoldier
      L.runQueueT (L.capture $ L.enemyAttacks mid p) >>= start
      choose "no defender" (isLabel "$label.noDefender")
      choose "EM Shield" (isAbilityOfCard shield)
      pass
      damageOf p `shouldReturn` 0

  it "Covert Surveillance collects the actual removal including Operative Skill's bonus" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      L.runQueueT (L.capture $ changeSuit p True) >>= runNow
      changeForm p
      pass
      skill <- putIntoPlay p "37013"
      clearHand p
      placeMainThreat 5
      event <- putInHand p "50038"
      resource <- putInHand p "01090"
      playWith event [resource]
      chooseMainScheme
      choose "Operative Skill" (isAbilityOfCard skill)
      choose "collect actual removal" (isLabel "$label.changeSuit")
      pass
      mainSchemeThreatNow `shouldReturn` 2
      suitThreat p `shouldReturn` 3
