module Marvel.Hero.IronheartSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Identity ( currentIdentity, ironheartVersion )
import Marvel.Hero.Runner qualified as Hero
import TestImport

ironheartGame :: IO TestGame
ironheartGame = testGame ["ironheart"]

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

progress :: IdentityId -> Natural -> GameAppT ()
progress p n = runNow [PlaceCounters (toTarget p) n]

levelUp :: GameAppT ()
levelUp = choose "Level Up!" (isNamedAbility "Level Up!")

childProdigy :: GameAppT ()
childProdigy = choose "Child Prodigy" (isNamedAbility "Child Prodigy")

heroThwart :: IdentityId -> GameAppT Natural
heroThwart p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> Hero.getModifiedThwart (toAttrs h)
  _ -> pure 0

spec :: Spec
spec = describe "Ironheart" do
  describe "progressing identities (29001–29003)" do
    it "begins as version 1 Riri, with only that identity pair available" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        ironheartVersion p `shouldReturn` 1
        handSizeOf p `shouldReturn` 6
        a <- playerAttrs p
        a.playerIdentitySide `shouldBe` B
        HashMap.size a.playerIdentitySides `shouldBe` 2
        hitPointsOf p `shouldReturn` 10

    it "version 1 has THW 1, ATK 2, DEF 3 and hand size 4" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        heroThwart p `shouldReturn` 1
        handSizeOf p `shouldReturn` 4

    it "cannot level up with fewer than six progress counters" do
      tg <- ironheartGame
      inGame tg do
        asHero (player1 tg)
        progress (player1 tg) 5
        offers (isNamedAbility "Level Up!") `shouldReturn` False

    it "leveling to version 2 spends six counters, readies, and preserves damage, status and attachments" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        jets <- putIntoPlay p "29013"
        setDamage p 3
        runNow [IdentityMessage p ExhaustedIdentity, IdentityMessage p IdentityConfused, IdentityMessage p IdentityBecomeTough]
        progress p 8
        levelUp
        ironheartVersion p `shouldReturn` 2
        countersOf p `shouldReturn` 2
        exhaustedNow p `shouldReturn` False
        damageOf p `shouldReturn` 3
        a <- playerAttrs p
        a.playerIdentityConfused `shouldBe` True
        a.playerIdentityTough `shouldBe` True
        toCardCode jets `shouldBe` "29013"
        hitPointsOf p `shouldReturn` 12
        handSizeOf p `shouldReturn` 5
        heroThwart p `shouldReturn` 2

    it "leveling to version 3 grants tough, THW 3 and hand size 6" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        progress p 12
        levelUp
        levelUp
        ironheartVersion p `shouldReturn` 3
        countersOf p `shouldReturn` 0
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        heroThwart p `shouldReturn` 3
        handSizeOf p `shouldReturn` 6
        offers (isNamedAbility "Level Up!") `shouldReturn` False

    it "changes to the alter-ego of its current version" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        progress p 6
        levelUp
        runNow [IdentityMessage p (ChangedToForm B)]
        ironheartVersion p `shouldReturn` 2
        handSizeOf p `shouldReturn` 6
        a <- playerAttrs p
        a.playerIdentitySide `shouldBe` B

  describe "Child Prodigy" do
    it "spends a mental resource for one progress counter, once per round" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        mental <- putInHand p "01091"
        childProdigy
        payWith [mental]
        countersOf p `shouldReturn` 1
        offers (isNamedAbility "Child Prodigy") `shouldReturn` False

    it "does not offer version 1's ability when only physical resources are available" do
      tg <- ironheartGame
      inGame tg do
        clearHand (player1 tg)
        _ <- putInHand (player1 tg) "01090"
        offers (isNamedAbility "Child Prodigy") `shouldReturn` False

    it "permits the new version's Child Prodigy after using the earlier version that round" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        mental <- putInHand p "01091"
        childProdigy
        payWith [mental]
        asHero p
        progress p 5
        levelUp
        runNow [IdentityMessage p (ChangedToForm B)]
        _ <- putInHand p "01091"
        offers (isNamedAbility "Child Prodigy") `shouldReturn` True

    it "version 2 can pay two resources of any type" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        progress p 6
        levelUp
        runNow [IdentityMessage p (ChangedToForm B)]
        strength <- putInHand p "01090"
        childProdigy
        payWith [strength]
        countersOf p `shouldReturn` 1
        offers (isNamedAbility "Child Prodigy") `shouldReturn` False

    it "version 3 accepts one resource of any type" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        progress p 12
        levelUp
        levelUp
        runNow [IdentityMessage p (ChangedToForm B)]
        resource <- putInHand p "01092"
        childProdigy
        payWith [resource]
        countersOf p `shouldReturn` 1

  describe "Maximum Efficiency" do
    it "spends progress to deal two non-attack damage even while stunned" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        progress p 13
        levelUp
        levelUp
        runNow [IdentityMessage p IdentityStunned]
        choose "Maximum Efficiency" (isNamedAbility "Maximum Efficiency")
        chooseVillain
        countersOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 2
        (.playerIdentityStunned) <$> playerAttrs p `shouldReturn` True
        offers (isNamedAbility "Maximum Efficiency") `shouldReturn` False
