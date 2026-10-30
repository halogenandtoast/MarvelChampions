module Marvel.Aspect.IronheartLeadershipSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Phase
import TestImport

asIronheart :: IdentityId -> GameAppT ()
asIronheart p = changeForm p >> pass >> clearHand p

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code money = do
  card <- putInHand p code
  payment <- traverse (putInHand p) money
  playWith card payment
  pure card

printedAbility :: PlayerCard -> Choice -> Bool
printedAbility card choice = isAbilityOfCard card choice && case choice of
  UseAbility a -> abilityIndex a == 1
  _ -> False

heroPowers :: IdentityId -> GameAppT [Natural]
heroPowers p = currentIdentity <$> getPlayer p >>= \case
  HeroSide hero -> sequence [Hero.getModifiedThwart (toAttrs hero), Hero.getModifiedAttack (toAttrs hero), Hero.getModifiedDefense (toAttrs hero)]
  AlterEgoSide _ -> pure []

allyThwartPower :: CardCode -> GameAppT Natural
allyThwartPower code = allyIn code >>= Ally.getModifiedThwart

allyAttackPower :: CardCode -> GameAppT Natural
allyAttackPower code = allyIn code >>= Ally.getModifiedAttack

spec :: Spec
spec = describe "Ironheart Leadership" do
  describe "Cloud 9" do
    it "exhausts to improve only the selected player's Aerial characters for the phase" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = seats tg !! 1
      inGame tg do
        asIronheart p
        _ <- putIntoPlay p "29015"
        _ <- putIntoPlay p "01067"
        _ <- putIntoPlay other "29024"
        cloud <- paying p "29014" ["01088", "01089"]
        choose "Cloud 9 action" (printedAbility cloud)
        choose "select controller" (isTargetChoice p)
        pass
        allyThwartPower "29015" `shouldReturn` 2
        allyThwartPower "01067" `shouldReturn` 2
        heroPowers p `shouldReturn` [1, 2, 3]
        clouds <- alliesWith "29014"
        allyThwartPower "29024" `shouldReturn` 2
        map allyExhausted (filter ((== p) . allyController) clouds) `shouldBe` [True]
        runNow [EndPhase PlayerPhase]
        allyThwartPower "29015" `shouldReturn` 1

    it "also affects Aerial characters that enter play later in the phase" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        cloud <- paying p "29014" ["01088", "01089"]
        choose "Cloud 9 action" (printedAbility cloud)
        choose "select controller" (isTargetChoice p)
        pass
        _ <- putIntoPlay p "29015"
        allyThwartPower "29015" `shouldReturn` 2

    it "cannot use its Hero Action in alter ego" do
      tg <- testGame ["ironheart"]
      inGame tg do
        cloud <- putIntoPlay (player1 tg) "29014"
        offers (printedAbility cloud) `shouldReturn` False

  describe "Falcon" do
    it "spends energy after attacking to ready another controlled Champion" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        falcon <- paying p "29015" ["01088", "01089"]
        resolve [IdentityMessage p ExhaustedIdentity]
        energy <- putInHand p "01088"
        a <- allyIn "29015"
        choose "Falcon attack" (isAllyAttack a)
        chooseVillain
        choose "Falcon response" (printedAbility falcon)
        payWith [energy]
        choose "ready Ironheart" (isTargetChoice p)
        pass
        exhaustedNow p `shouldReturn` False
        allyExhausted <$> allyIn "29015" `shouldReturn` True
        allyDamage <$> allyIn "29015" `shouldReturn` 1

    it "can respond after thwarting but cannot ready itself or another player's character" do
      tg <- testGame ["ironheart", "nova"]
      let p = player1 tg
          other = seats tg !! 1
      inGame tg do
        asIronheart p
        falcon <- paying p "29015" ["01088", "01089"]
        own <- putIntoPlay p "29004"
        foreignAlly <- putIntoPlay other "29014"
        resolve [AllyMessage (allyIdOf own) ExhaustedAlly, AllyMessage (allyIdOf foreignAlly) ExhaustedAlly]
        energy <- putInHand p "01088"
        placeMainThreat 4
        a <- allyIn "29015"
        choose "Falcon thwart" (isAllyThwart a)
        chooseMainScheme
        choose "Falcon response" (printedAbility falcon)
        payWith [energy]
        offers (isTargetChoice $ allyIdOf falcon) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf foreignAlly) `shouldReturn` False
        choose "ready own Champion" (isTargetChoice $ allyIdOf own)
        pass
        ownAllies <- alliesWith "29004"
        map allyExhausted (filter ((== p) . allyController) ownAllies) `shouldBe` [False]
        allyExhausted <$> allyIn "29014" `shouldReturn` True

    it "requires energy and Hero form for its response" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        falcon <- putIntoPlay p "29015"
        own <- putIntoPlay p "29004"
        resolve [AllyMessage (allyIdOf own) ExhaustedAlly]
        _ <- putInHand p "01088"
        a <- allyIn "29015"
        choose "Falcon attack in alter ego" (isAllyAttack a)
        chooseVillain
        offers (printedAbility falcon) `shouldReturn` False
        pass
        asIronheart p
        resolve [AllyMessage (allyIdOf falcon) ReadiedAlly]
        _ <- putInHand p "01090"
        b <- allyIn "29015"
        choose "Falcon attack without energy" (isAllyAttack b)
        chooseVillain
        offers (printedAbility falcon) `shouldReturn` False

  describe "Patriot" do
    it "boosts all basic hero powers until the end of the round, including alter-ego recovery" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        patriot <- paying p "29016" ["01090", "01092"]
        choose "Patriot response" (printedAbility patriot)
        choose "boost Ironheart" (isTargetChoice p)
        pass
        heroPowers p `shouldReturn` [2, 3, 4]
        resolve [IdentityMessage p (ChangedToForm B)]
        currentIdentity <$> getPlayer p >>= \case
          AlterEgoSide ae -> AlterEgo.getModifiedRecovery (toAttrs ae) `shouldReturn` 4
          _ -> error "expected alter ego"
        runNow [EndRound]
        currentIdentity <$> getPlayer p >>= \case
          AlterEgoSide ae -> AlterEgo.getModifiedRecovery (toAttrs ae) `shouldReturn` 3
          _ -> error "expected alter ego"

    it "can select another player's Champion and excludes non-Champions" do
      tg <- testGame ["ironheart", "nova"]
      let p = player1 tg
          other = seats tg !! 1
      inGame tg do
        asIronheart p
        champion <- putIntoPlay other "29004"
        ordinary <- putIntoPlay p "01067"
        patriot <- paying p "29016" ["01090", "01092"]
        choose "Patriot response" (printedAbility patriot)
        offers (isTargetChoice $ allyIdOf ordinary) `shouldReturn` False
        choose "boost another player's Brawn" (isTargetChoice $ allyIdOf champion)
        pass
        allyAttackPower "29004" `shouldReturn` 4
        allyThwartPower "29004" `shouldReturn` 3

    it "does not offer its Hero Response while entering play in alter ego" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        patriot <- paying p "29016" ["01090", "01092"]
        offers (printedAbility patriot) `shouldReturn` False
        heroPowers p `shouldReturn` []

  describe "Go All Out" do
    it "exhausts Ironheart and deals the total of her modified THW, ATK, and DEF" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        _ <- paying p "05032" ["29014"]
        choose "Morale Boost Ironheart" (isTargetChoice p)
        pass
        _ <- paying p "29017" ["01088"]
        chooseVillain
        pass
        exhaustedNow p `shouldReturn` True
        villainDamageNow `shouldReturn` 9

    it "requires an energy resource even with enough other resources" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        card <- putInHand p "29017"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` True
        resolve [IdentityMessage p ExhaustedIdentity]
        offers (isPlay card) `shouldReturn` False

    it "spends the cost but clears stun instead of dealing damage" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        resolve [IdentityMessage p IdentityStunned]
        _ <- paying p "29017" ["01088"]
        pass
        isStunned p `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        villainDamageNow `shouldReturn` 0

  describe "Push Ahead" do
    it "exhausts Ironheart and removes threat equal to her modified basic powers" do
      tg <- testGame ["ironheart", "spider-man", "nova"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        placeMainThreat 12
        _ <- paying p "05032" ["29014"]
        choose "Morale Boost Ironheart" (isTargetChoice p)
        pass
        before <- mainSchemeThreat
        _ <- paying p "29018" ["01089", "01092"]
        chooseMainScheme
        pass
        mainSchemeThreat `shouldReturn` (before - 9)
        exhaustedNow p `shouldReturn` True

    it "requires mental payment and a ready hero" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        placeMainThreat 5
        card <- putInHand p "29018"
        _ <- putInHand p "01088"
        _ <- putInHand p "01092"
        offers (isPlay card) `shouldReturn` False
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` True
        resolve [IdentityMessage p ExhaustedIdentity]
        offers (isPlay card) `shouldReturn` False

    it "clears confuse without removing threat" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        placeMainThreat 5
        before <- mainSchemeThreat
        resolve [IdentityMessage p IdentityConfused]
        _ <- paying p "29018" ["01089", "01092"]
        pass
        isConfused p `shouldReturn` False
        mainSchemeThreat `shouldReturn` before
        exhaustedNow p `shouldReturn` True

  describe "R&D Facility" do
    it "requires two mental resources and starts with three research counters" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        facility <- putInHand p "29020"
        oneMental <- putInHand p "29014"
        physical <- putInHand p "01090"
        offers (isPlay facility) `shouldReturn` False
        mental <- putInHand p "29011"
        playWith facility [oneMental, physical, mental]
        supportUses . head <$> supportsWith "29020" `shouldReturn` 3
        offers (printedAbility facility) `shouldReturn` False

    it "exhausts and spends a counter to improve any friendly character until phase end" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = seats tg !! 1
      inGame tg do
        asIronheart p
        ally <- putIntoPlay other "01067"
        facility <- paying p "29020" ["01089", "29014"]
        choose "R&D Facility" (printedAbility facility)
        choose "improve another player's ally" (isTargetChoice $ allyIdOf ally)
        pass
        allyThwartPower "01067" `shouldReturn` 3
        allyAttackPower "01067" `shouldReturn` 2
        supportUses . head <$> supportsWith "29020" `shouldReturn` 2
        supportExhausted . head <$> supportsWith "29020" `shouldReturn` True
        offers (printedAbility facility) `shouldReturn` False
        runNow [EndPhase PlayerPhase]
        allyThwartPower "01067" `shouldReturn` 2
        allyAttackPower "01067" `shouldReturn` 1

    it "discards after its third use while the final improvement lasts through the phase" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asIronheart p
        facility <- paying p "29020" ["01089", "29014"]
        for_ [1 :: Int, 2, 3] \n -> do
          when (n > 1) $ resolve [SupportMessage (supportIdOf facility) ReadiedSupport]
          choose "R&D Facility" (printedAbility facility)
          choose "improve Ironheart" (isTargetChoice p)
          pass
        supportsWith "29020" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem facility)
        heroPowers p `shouldReturn` [4, 5, 3]
        runNow [EndPhase PlayerPhase]
        heroPowers p `shouldReturn` [1, 2, 3]
