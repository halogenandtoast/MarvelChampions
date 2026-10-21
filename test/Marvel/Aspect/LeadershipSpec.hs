module Marvel.Aspect.LeadershipSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types (AbilityType (Action))
import Marvel.Minion.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "Leadership" do
  describe "Hawkeye (01066)" do
    it "enters play with 4 arrows, one of which deals 2 damage to a minion entering play" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01066"
        hawkeye <- allyIn "01066"
        hawkeye.allyCounters `shouldBe` 4
        card <- encounterCard Cards.hydraMercenary
        runNow [RevealEncounterCard p card]
        choose "use Hawkeye" (isAbilityOf (allyId hawkeye))
        [mercenary] <- getsGame (HashMap.keys . gameMinions)
        minionDamageOf mercenary `shouldReturn` Just 2
        (.allyCounters) <$> allyIn "01066" `shouldReturn` 3

  describe "Maria Hill (01067)" do
    it "has each player draw 1 card after she enters play" do
      tg <- testGame ["captain-marvel", "spider-man"]
      let (p, q) = (player1 tg, player2 tg)
      inGame tg do
        clearHand p
        maria <- putInHand p "01067"
        energy <- putInHand p "01088"
        others <- length <$> handOf q
        playWith maria [energy]
        choose "use Maria Hill" \case
          UseAbility ab -> abilitySource ab /= toSource p
          _ -> False
        length <$> handOf p `shouldReturn` 1
        length <$> handOf q `shouldReturn` (others + 1)

  describe "Vision (01068)" do
    it "spends an energy resource for +2 ATK until the end of the phase, once a round" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01068"
        vision <- allyIn "01068"
        energy <- putInHand p "01088"
        _ <- putInHand p "01088"
        choose "use Vision" \case
          UseAbility ab -> abilitySource ab == toSource (allyId vision) && abilityType ab == Action
          _ -> False
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        choose "ATK" (isLabel "$label.attackPlusOne")
        offers \case
          UseAbility ab -> abilitySource ab == toSource (allyId vision) && abilityType ab == Action
          _ -> False
          `shouldReturn` False
        villain <- getVillain
        choose "attack with Vision" (isAllyAttack vision)
        choose "Rhino" (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 4

  describe "Get Ready (01069)" do
    it "readies an exhausted ally" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        getReady <- putInHand p "01069"
        offers (isPlay getReady) `shouldReturn` False
        putIntoPlay p "01067"
        maria <- allyIn "01067"
        villain <- getVillain
        choose "attack with Maria Hill" (isAllyAttack maria)
        choose "Rhino" (isTargetChoice (toId villain))
        (.allyExhausted) <$> allyIn "01067" `shouldReturn` True
        choose "play Get Ready" (isPlay getReady)
        chooseIf (isTargetChoice (allyId maria))
        (.allyExhausted) <$> allyIn "01067" `shouldReturn` False

  describe "Lead from the Front (01070)" do
    it "gives each character a player controls +1 THW and +1 ATK until the end of the phase" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01067"
        maria <- allyIn "01067"
        lead <- putInHand p "01070"
        energy <- putInHand p "01088"
        offers (isPlay lead) `shouldReturn` False
        changeForm p
        pass
        playWith lead [energy]
        chooseIf (isTargetChoice p)
        villain <- getVillain
        choose "attack with Maria Hill" (isAllyAttack maria)
        choose "Rhino" (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 2
        choose "attack" (isHeroAttack p)
        choose "Rhino" (isTargetChoice (toId villain))
        -- Captain Marvel has 2 ATK
        villainDamageNow `shouldReturn` 5

  describe "Make the Call (01071)" do
    it "pays an ally's printed cost to put it into play from any discard pile under your control" do
      tg <- testGame ["captain-marvel", "spider-man"]
      let (p, q) = (player1 tg, player2 tg)
      inGame tg do
        clearHand p
        call <- putInHand p "01071"
        offers (isPlay call) `shouldReturn` False
        maria <- putInDiscard q "01067"
        energy <- putInHand p "01088"
        choose "play Make the Call" (isPlay call)
        choose "call Maria Hill" (isPlay maria)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        [m] <- alliesWith "01067"
        m.allyController `shouldBe` p
        map toCardCode <$> discardOf q `shouldReturn` []

  describe "The Power of Leadership (01072)" do
    it "pays 2 for a Leadership card" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        maria <- putInHand p "01067"
        power <- putInHand p "01072"
        playWith maria [power]
        pass
        alliesWith "01067" >>= (`shouldSatisfy` ((== 1) . length))

  describe "The Triskelion (01073)" do
    it "costs 1 and raises the ally limit to 4" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        triskelion <- putInHand p "01073"
        genius <- putInHand p "01089"
        playWith triskelion [genius]
        supportsWith "01073" >>= (`shouldSatisfy` ((== 1) . length))
        for_ ["01066", "01067", "01068"] (putIntoPlay p)
        fury <- putInHand p "01084"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith fury [strength, energy]
        offers (isTargetChoice p) `shouldReturn` False
        length <$> getsGame (HashMap.elems . gameAllies) `shouldReturn` 4

  describe "Inspired (01074)" do
    it "attaches to an ally for +1 THW and +1 ATK, max 1 per ally" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        inspired <- putInHand p "01074"
        offers (isPlay inspired) `shouldReturn` False
        putIntoPlay p "01067"
        maria <- allyIn "01067"
        genius <- putInHand p "01089"
        playWith inspired [genius]
        chooseIf (isTargetChoice (allyId maria))
        [u] <- upgradesWith "01074"
        u.upgradeAttachedAlly `shouldBe` Just (allyId maria)
        -- another copy cannot go on Maria Hill
        inspired2 <- putInHand p "01074"
        _ <- putInHand p "01089"
        offers (isPlay inspired2) `shouldReturn` False
        villain <- getVillain
        choose "attack with Maria Hill" (isAllyAttack maria)
        choose "Rhino" (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 2

  describe "the ally limit" do
    it "makes a player with more than 3 allies discard one" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        for_ ["01066", "01067", "01068"] (putIntoPlay p)
        fury <- putInHand p "01084"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith fury [strength, energy]
        hawkeye <- allyIn "01066"
        choose "discard Hawkeye" (isTargetChoice (allyId hawkeye))
        length <$> getsGame (HashMap.elems . gameAllies) `shouldReturn` 3
        alliesWith "01066" `shouldReturn` []
