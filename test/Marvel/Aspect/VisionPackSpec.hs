module Marvel.Aspect.VisionPackSpec (spec) where

import Marvel.ActiveCost (allocationLabel)
import Marvel.Ally.Runner qualified as Ally
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  chooseIf isNoResponse

isAllyDefending :: PlayerCard -> Choice -> Bool
isAllyDefending card = \case
  AllyDefend aid _ -> aid == allyIdOf card
  _ -> False

spec :: Spec
spec = describe "Vision pack" do
  describe "Jocasta" do
    it "attaches a Defense event from discard and makes it playable as a hand card" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        defense <- putInDiscard p "26018"
        jocasta <- putInHand p "26013"
        resources <- traverse (putInHand p) ["01088", "01093"]
        playWith jocasta resources
        choose "Jocasta response" (isAbilityOfCard jocasta)
        choose "attach Defiance" (isTargetChoice defense)
        pass
        getAllyHeldCards p `shouldReturn` [defense]
        rhinoAttacks p
        choose "take the attack" isNoDefender
        choose "play held Defiance" (isPlay defense)
        pass
        getAllyHeldCards p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem defense)

  describe "Victor Mancha" do
    it "reduces attack damage by one but not damage from an ability" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        victor <- putInHand p "26015"
        resource <- putInHand p "01088"
        playWith victor [resource]
        pass
        rhinoAttacks p
        choose "Victor defends" (isAllyDefending victor)
        pass
        allyDamage <$> allyIn "26015" `shouldReturn` 1
        resolve [AllyMessage (allyIdOf victor) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
        allyDamage <$> allyIn "26015" `shouldReturn` 3

  describe "Protector" do
    it "spends a mental resource to reduce incoming damage once per round" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        protector <- putInHand p "26014"
        resources <- traverse (putInHand p) ["01088", "01090"]
        playWith protector resources
        pass
        mental <- putInHand p "01089"
        rhinoAttacks p
        choose "Protector defends" (isAllyDefending protector)
        choose "Protector interrupt" (isAbilityOfCard protector)
        payWith [mental]
        pass
        allyDamage <$> allyIn "26014" `shouldReturn` 1

  describe "Reboot" do
    it "readies another player's Android and heals one damage" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        android <- putIntoPlay other "01068"
        resolve [AllyMessage (allyIdOf android) ExhaustedAlly, AllyMessage (allyIdOf android) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
        reboot <- putInHand p "26024"
        resource <- putInHand p "01093"
        playOn reboot (AllyCharacter $ allyIdOf android) [resource]
        pass
        a <- allyIn "01068"
        allyExhausted a `shouldBe` False
        allyDamage a `shouldBe` 1

  describe "Defiance and Flow Like Water" do
    it "discards the boost card instead of turning it faceup and answers the Defense play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        flow <- putInHand p "26016"
        resource <- putInHand p "01088"
        playWith flow [resource]
        pass
        defense <- putInHand p "26018"
        rhinoAttacks p
        choose "take the attack" isNoDefender
        choose "Defiance" (isPlay defense)
        choose "Flow Like Water" (isAbilityOfCard flow)
        pass
        villainDamageNow `shouldReturn` 1
        damageOf p `shouldReturn` 2

  describe "Joining Forces" do
    it "puts an Avenger and Guardian into play from different players' hands" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        clearHand p
        clearHand other
        avenger <- putInHand p "01068"
        guardian <- putInHand other "17020"
        joining <- putInHand p "26035"
        resources <- traverse (putInHand p) ["01088", "01089"]
        playWith joining resources
        choose "Vision" (isTargetChoice avenger)
        choose "Cosmo" (isTargetChoice guardian)
        pass
        allyController <$> allyIn "01068" `shouldReturn` p
        allyController <$> allyIn "17020" `shouldReturn` other

  describe "Machine Man" do
    it "spends resources for this attack and resets the bonus afterward" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        machine <- putIntoPlay p "26022"
        clearHand p
        energy <- putInHand p "01088"
        a <- allyIn "26022"
        choose "Machine Man attacks" (isAllyAttack a)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        choose "Machine Man interrupt" (isAbilityOfCard machine)
        choose "spend two resources" (isPayWith energy)
        whenM (offers isFinishPayment) $ choose "finish spending" isFinishPayment
        choose "spend both generated Energy resources" (isLabel $ allocationLabel [Energy, Energy])
        pass
        villainDamageNow `shouldReturn` 3
        a' <- allyIn "26022"
        Ally.getModifiedAttack a' `shouldReturn` 1
        allyDamage a' `shouldBe` 1

    it "declines the optional boost for an ordinary unboosted attack" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        machine <- putIntoPlay p "26022"
        clearHand p
        a <- allyIn "26022"
        choose "Machine Man attacks" (isAllyAttack a)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        offers (isAbilityOfCard machine) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 1

  describe "Assault Training" do
    it "shuffles only your Aggression event from discard and exhausts" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "26033"
        clearHand p
        event <- putInDiscard p "01054"
        _ <- putInDiscard p "01044"
        choose "Assault Training" (isAbilityOfCard training)
        choose "Uppercut" (isTargetChoice event)
        pass
        discardOf p >>= (`shouldSatisfy` notElem event)
        offers (isAbilityOfCard training) `shouldReturn` False

  describe "Chance Encounter" do
    it "searches an ally when its attached scheme is defeated" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        scheme <- revealNow p Schemes.bombScare
        pass
        let sid = sideSchemeOf scheme
        ally <- putInDiscard p "01067"
        encounter <- putInHand p "26034"
        playOn encounter sid []
        runNow [SideSchemeMessage sid (SideSchemeThwarted GameRef 3)]
        choose "Chance Encounter interrupt" (isAbilityOfCard encounter)
        choose "find discarded ally" (isTargetChoice ally)
        pass
        handOf p >>= (`shouldSatisfy` elem ally)

    it "allows a copy on each of two different side schemes" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        first <- revealNow p Schemes.bombScare
        pass
        second <- revealNow p Schemes.crowdControl
        pass
        one <- putInHand p "26034"
        playOn one (sideSchemeOf first) []
        pass
        two <- putInHand p "26034"
        offers (isPlay two) `shouldReturn` True
        playOn two (sideSchemeOf second) []
        pass
        length <$> upgradesWith "26034" `shouldReturn` 2
