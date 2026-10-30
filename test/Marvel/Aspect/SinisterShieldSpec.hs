module Marvel.Aspect.SinisterShieldSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Side (Side (B))
import Marvel.Event.Cards qualified as Events
import Marvel.Minion.Cards qualified as Minions
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

isResponseOf :: PlayerCard -> Choice -> Bool
isResponseOf card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

spec :: Spec
spec = describe "Sinister Motives SHIELD and basic allies" do
  describe "Field Agent" do
    it "prevents one consequential damage from a SHIELD ally's basic attack" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        agent <- putIntoPlay p "27044"
        _ <- putIntoPlay p "01075"
        ally <- allyIn "01075"
        choose "attack with Black Widow" (isAllyAttack ally)
        chooseVillain
        choose "use Field Agent" (isAbilityOfCard agent)
        pass
        allyDamage <$> allyIn "01075" `shouldReturn` 0
        map supportUses <$> supportsWith "27044" `shouldReturn` [2]
        map supportExhausted <$> supportsWith "27044" `shouldReturn` [True]

    it "can protect another player's SHIELD ally" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        agent <- putIntoPlay p "27044"
        ally <- putIntoPlay other "01075"
        start $ damageAlly (allyIdOf ally) GameRef (toDamage 1 FromConsequential)
        choose "protect the other player's ally" (isAbilityOfCard agent)
        pass
        allyDamage <$> allyIn "01075" `shouldReturn` 0

    it "does not prevent attack damage, other ability damage, non-SHIELD consequential damage, or trigger in alter-ego" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        agent <- putIntoPlay p "27044"
        ally <- putIntoPlay p "01075"
        for_ [FromAttack, FromAbility] \source -> do
          start $ damageAlly (allyIdOf ally) GameRef (toDamage 1 source)
          offers (isAbilityOfCard agent) `shouldReturn` False
          pass
        nonShield <- putIntoPlay p "01068"
        start $ damageAlly (allyIdOf nonShield) GameRef (toDamage 1 FromConsequential)
        offers (isAbilityOfCard agent) `shouldReturn` False
        pass
        resolve [IdentityMessage p (ChangedToForm B)]
        another <- putIntoPlay p "01075"
        start $ damageAlly (allyIdOf another) GameRef (toDamage 1 FromConsequential)
        offers (isAbilityOfCard agent) `shouldReturn` False

    it "discards itself when its third backup counter is spent" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        agent <- putIntoPlay p "27044"
        ally <- putIntoPlay p "01075"
        for_ [1 :: Int, 2, 3] \_ -> do
          start $ damageAlly (allyIdOf ally) GameRef (toDamage 1 FromConsequential)
          choose "Field Agent" (isAbilityOfCard agent)
          pass
          resolve [SupportMessage (supportIdOf agent) ReadiedSupport]
        supportsWith "27044" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem agent)

  describe "Agent 13" do
    for_ ["attack", "thwart"] \power ->
      it ("readies another player's SHIELD support after her " <> power) do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          asHero p
          agent <- putIntoPlay p "27046"
          support <- putIntoPlay other "01064"
          nonShield <- putIntoPlay p "27035"
          resolve [SupportMessage (supportIdOf support) ExhaustedSupport, SupportMessage (supportIdOf nonShield) ExhaustedSupport]
          attrs <- allyIn "27046"
          if power == "attack"
            then choose "attack" (isAllyAttack attrs) >> chooseVillain
            else placeMainThreat 4 >> choose "thwart" (isAllyThwart attrs) >> chooseMainScheme
          choose "Agent 13's response" (isResponseOf agent)
          offers (isTargetChoice $ supportIdOf nonShield) `shouldReturn` False
          choose "the other player's Surveillance Team" (isTargetChoice $ supportIdOf support)
          pass
          map supportExhausted <$> supportsWith "01064" `shouldReturn` [False]
          map supportExhausted <$> supportsWith "27035" `shouldReturn` [True]

  describe "Dum Dum Dugan" do
    for_ ["attack", "thwart"] \power ->
      it ("exhausts up to three controlled SHIELD cards to increase this " <> power <> " only") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          asHero p
          dugan <- putIntoPlay p "27047"
          own <- traverse (putIntoPlay p) ["01064", "01080", "01056"]
          opponents <- putIntoPlay other "01064"
          nonShield <- putIntoPlay p "27035"
          attrs <- allyIn "27047"
          before <- if power == "attack" then villainDamageNow else placeMainThreat 9 >> mainSchemeThreatNow
          if power == "attack"
            then choose "attack" (isAllyAttack attrs) >> chooseVillain
            else choose "thwart" (isAllyThwart attrs) >> chooseMainScheme
          choose "Dugan's interrupt" (isResponseOf dugan)
          offers (isTargetChoice $ allyIdOf dugan) `shouldReturn` False
          offers (isTargetChoice $ supportIdOf opponents) `shouldReturn` False
          offers (isTargetChoice $ supportIdOf nonShield) `shouldReturn` False
          for_ own \card -> choose "exhaust a SHIELD card" (isTargetChoice $ supportIdOf card)
          pass
          if power == "attack"
            then villainDamageNow `shouldReturn` before + 6
            else mainSchemeThreatNow `shouldReturn` before - 6
          allyDamage <$> allyIn "27047" `shouldReturn` (if power == "attack" then 2 else 3)

    it "may exhaust zero cards and does not retain a previous power's bonus" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        dugan <- putIntoPlay p "27047"
        support <- putIntoPlay p "01064"
        attrs <- allyIn "27047"
        choose "attack" (isAllyAttack attrs)
        chooseVillain
        choose "Dugan's interrupt" (isResponseOf dugan)
        choose "exhaust Surveillance Team" (isTargetChoice $ supportIdOf support)
        pass
        villainDamageNow `shouldReturn` 4
        resolve [AllyMessage (allyIdOf dugan) ReadiedAlly, SupportMessage (supportIdOf support) ReadiedSupport]
        attrs' <- allyIn "27047"
        choose "attack again" (isAllyAttack attrs')
        chooseVillain
        choose "Dugan's interrupt again" (isResponseOf dugan)
        choose "exhaust nothing" (isLabel "$label.done")
        pass
        villainDamageNow `shouldReturn` 7

  describe "Government Liaison" do
    it "plays a SHIELD card from hand with a reduction confined to that play" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        liaison <- putIntoPlay p "27054"
        team <- putInHand p "27045"
        resource <- putInHand p "27031"
        nonShield <- putInHand p "27035"
        refreshTurn
        choose "Government Liaison" (isAbilityOfCard liaison)
        offers (isTargetChoice nonShield) `shouldReturn` False
        choose "play Surveillance Team" (isTargetChoice team)
        payWith [resource]
        pass
        supportsWith "27045" >>= (`shouldSatisfy` ((== 1) . length))
        clearHand p
        another <- putInHand p "27045"
        _ <- putInHand p "27031"
        refreshTurn
        offers (isPlay another) `shouldReturn` False
        map supportExhausted <$> supportsWith "27054" `shouldReturn` [True]

    it "cannot be used in alter-ego or without a card playable after the reduction" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        liaison <- putIntoPlay p "27054"
        _ <- putInHand p "27045"
        offers (isAbilityOfCard liaison) `shouldReturn` False
        changeForm p
        pass
        offers (isAbilityOfCard liaison) `shouldReturn` False

  describe "Sky-Destroyer" do
    it "can respond to its own play and deals two damage after exhausting" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        sky <- putInHand p "27055"
        resources <- traverse (putInHand p) ["01088", "27031"]
        playWith sky resources
        choose "Sky-Destroyer's response" (isAbilityOfCard sky)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        map supportExhausted <$> supportsWith "27055" `shouldReturn` [True]

    it "does not respond to putting a SHIELD card into play or playing a non-SHIELD card" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        sky <- putIntoPlay p "27055"
        _ <- putIntoPlay p "01064"
        offers (isAbilityOfCard sky) `shouldReturn` False
        card <- putInHand p "27039"
        resource <- putInHand p "27031"
        playWith card [resource]
        offers (isAbilityOfCard sky) `shouldReturn` False

  describe "Ghost-Spider (Gwen Stacy)" do
    it "searches its controller's deck for an identity-specific event when returned to hand" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        ghost <- putIntoPlay p "27048"
        [signature, aspect, support] <- stackDeck p ["27034", "27042", "27035"]
        runNow [ReturnToHand (toRef $ allyIdOf ghost)]
        choose "Ghost-Spider's interrupt" (isResponseOf ghost)
        offers (isTargetChoice aspect) `shouldReturn` False
        offers (isTargetChoice support) `shouldReturn` False
        choose "find Web-Shot" (isTargetChoice signature)
        pass
        handOf p >>= (`shouldSatisfy` (\cards -> signature `elem` cards && ghost `elem` cards))
        unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` notElem signature)

    it "also searches when defeated by consequential damage" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        ghost <- putIntoPlay p "27048"
        [signature] <- stackDeck p ["27031"]
        start $ damageAlly (allyIdOf ghost) GameRef (toDamage 3 FromConsequential)
        choose "Ghost-Spider's interrupt" (isResponseOf ghost)
        choose "find Arachnobatics" (isTargetChoice signature)
        pass
        handOf p >>= (`shouldSatisfy` elem signature)
        discardOf p >>= (`shouldSatisfy` elem ghost)

    it "can be played with Web of Life and Destiny as the only controlled Web-Warrior card" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "27023"
        ghost <- putInHand p "27048"
        resources <- traverse (putInHand p) ["01088", "27031"]
        refreshTurn
        offers (isPlay ghost) `shouldReturn` True
        playWith ghost resources
        pass
        alliesWith "27048" >>= (`shouldSatisfy` ((== 1) . length))

    it "cannot be played without a controlled Web-Warrior card" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ghost <- putInHand p "27048"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        refreshTurn
        offers (isPlay ghost) `shouldReturn` False

  describe "Spider-Man (Peter Parker)" do
    it "requires energy, mental and physical resources even when three other resources are available" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asHero p
        peter <- putInHand p "27049"
        energy <- putInHand p "01088"
        mental <- putInHand p "27031"
        refreshTurn
        offers (isPlay peter) `shouldReturn` False
        physical <- putInHand p "27039"
        refreshTurn
        offers (isPlay peter) `shouldReturn` True
        playWith peter [energy, mental, physical]
        pass
        alliesWith "27049" >>= (`shouldSatisfy` ((== 1) . length))

    for_ ["attack", "thwart"] \power ->
      it ("readies another Web-Warrior character after a basic " <> power <> ", excluding itself") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          asHero p
          peter <- putIntoPlay p "27049"
          resolve [IdentityMessage p ExhaustedIdentity]
          attrs <- allyIn "27049"
          if power == "attack"
            then choose "attack" (isAllyAttack attrs) >> chooseVillain
            else placeMainThreat 4 >> choose "thwart" (isAllyThwart attrs) >> chooseMainScheme
          choose "Peter's response" (isResponseOf peter)
          offers (isTargetChoice $ allyIdOf peter) `shouldReturn` False
          choose "ready Miles" (isTargetChoice p)
          pass
          isExhaustedNow p `shouldReturn` False
          allyExhausted <$> allyIn "27049" `shouldReturn` True
