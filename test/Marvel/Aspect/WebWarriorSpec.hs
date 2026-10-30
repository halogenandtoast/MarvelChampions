module Marvel.Aspect.WebWarriorSpec (spec) where

import TestImport
import Marvel.Identity (currentIdentity)
import Marvel.Hero.Runner qualified as Hero
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Identity.Types (PlayerIdentitySide (..))

spec :: Spec
spec = describe "Sinister Motives basic cards" do
  describe "Plan B" do
    it "exhausts and discards a random hand card to deal nonattack damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        upgrade <- putIntoPlay p "27024"
        clearHand p
        fuel <- putInHand p "01088"
        refreshTurn
        choose "Plan B" (isAbilityOfCard upgrade)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        discardOf p >>= (`shouldSatisfy` elem fuel)
        offers (isAbilityOfCard upgrade) `shouldReturn` False

    it "cannot pay with an empty hand" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        upgrade <- putIntoPlay p "27024"
        clearHand p
        refreshTurn
        offers (isAbilityOfCard upgrade) `shouldReturn` False

  describe "Symbiote Suit" do
    it "increases all basic powers, hand size and hit points, and adds a hazard icon" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "27191"
        hitPointsOf p `shouldReturn` 20
        handSizeOf p `shouldReturn` 7
        getHazardCount `shouldReturn` 1
        currentIdentity <$> getPlayer p >>= \case
          AlterEgoSide ego -> AlterEgo.getModifiedRecovery (toAttrs ego) `shouldReturn` 4
          _ -> error "expected Peter Parker"
        changeForm p
        handSizeOf p `shouldReturn` 6
        currentIdentity <$> getPlayer p >>= \case
          HeroSide hero -> do
            Hero.getModifiedAttack (toAttrs hero) `shouldReturn` 3
            Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 2
            Hero.getModifiedDefense (toAttrs hero) `shouldReturn` 4
          _ -> error "expected Spider-Man"

  describe "Young Love" do
    it "heals both named alter-egos and shares its reprint" do
      tg <- testGame ["ghost-spider", "spider-man-miles-morales"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        setDamage p 5
        setDamage other 4
        clearHand p
        love <- putInHand p "27050"
        resource <- putInHand p "01088"
        refreshTurn
        playWith love [resource]
        pass
        damageOf p `shouldReturn` 2
        damageOf other `shouldReturn` 1

    it "requires both named characters in play" do
      tg <- testGame ["ghost-spider", "spider-man-miles-morales"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        resolve [IdentityMessage other ChooseHeroForm]
        love <- putInHand p "27019"
        refreshTurn
        offers (isPlay love) `shouldReturn` False
