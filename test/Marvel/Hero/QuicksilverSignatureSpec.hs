module Marvel.Hero.QuicksilverSignatureSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import TestImport

quicksilver :: IO TestGame
quicksilver = testGame ["quicksilver"]

-- | Quicksilver in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

-- | The hero's THW, ATK and DEF as modified.
heroStats :: IdentityId -> GameAppT (Natural, Natural, Natural)
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> do
      let a = toAttrs h
      (,,) <$> Hero.getModifiedThwart a <*> Hero.getModifiedAttack a <*> Hero.getModifiedDefense a
    AlterEgoSide _ -> pure (0, 0, 0)

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

spec :: Spec
spec = describe "Quicksilver's signature upgrades and support" do
  describe "Serval Industries (14007)" do
    it "shuffles 2 Quicksilver cards of the player's choice from the discard pile into the deck" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        serval <- putIntoPlay p "14007"
        [reflex, perception, _, _] <- stackDiscard p ["14008", "14010", "14011", "01088"]
        choose "use Serval Industries" (isAbilityOfCard serval)
        choose "Accelerated Reflex" (isTargetChoice reflex)
        choose "Hyper Perception" (isTargetChoice perception)
        map toCardCode <$> discardOf p `shouldReturn` ["14011", "01088"]
        deck <- deckOf p
        liftIO $ (reflex `elem` deck, perception `elem` deck) `shouldBe` (True, True)
        map (.supportExhausted) <$> supportsWith "14007" `shouldReturn` [True]

    it "shuffles as many as there are when there are fewer than 2" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        serval <- putIntoPlay p "14007"
        [reflex, _] <- stackDiscard p ["14008", "01088"]
        choose "use Serval Industries" (isAbilityOfCard serval)
        map toCardCode <$> discardOf p `shouldReturn` ["01088"]
        deckOf p >>= (`shouldSatisfy` elem reflex)

    it "is not offered without a Quicksilver card in the discard pile, nor in hero form" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        serval <- putIntoPlay p "14007"
        _ <- stackDiscard p ["01088", "01089"]
        offers (isAbilityOfCard serval) `shouldReturn` False
        _ <- stackDiscard p ["14008"]
        offers (isAbilityOfCard serval) `shouldReturn` True
        asHero p
        offers (isAbilityOfCard serval) `shouldReturn` False

  describe "Accelerated Reflex (14008)" do
    it "gives Quicksilver +1 DEF" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14008"
        heroStats p `shouldReturn` (1, 1, 2)

  describe "Hyper Perception (14010)" do
    it "gives Quicksilver +1 THW" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14010"
        heroStats p `shouldReturn` (2, 1, 1)

  describe "Reinforced Sinew (14011)" do
    it "gives Quicksilver +1 ATK" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        sinew <- putInHand p "14011"
        genius <- putInHand p "01089"
        playWith sinew [genius]
        heroStats p `shouldReturn` (1, 2, 1)
        choose "attack" (isHeroAttack p)
        chooseVillain
        villainDamageNow `shouldReturn` 2

  describe "Friction Resistance (14009)" do
    it "readies after Quicksilver is readied" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        friction <- putIntoPlay p "14009"
        resolve [IdentityMessage p ExhaustedIdentity, UpgradeMessage (upgradeIdOf friction) ExhaustedUpgrade]
        runNow [IdentityMessage p ReadiedIdentity]
        choose "ready Friction Resistance" (isAbilityOfCard friction)
        upgradeIsExhausted (upgradeIdOf friction) `shouldReturn` False

    it "does not ready when Quicksilver was already ready" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        friction <- putIntoPlay p "14009"
        resolve [UpgradeMessage (upgradeIdOf friction) ExhaustedUpgrade]
        runNow [IdentityMessage p ReadiedIdentity]
        offers (isAbilityOfCard friction) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf friction) `shouldReturn` True

    it "is exhausted to generate a [physical] resource" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        friction <- putIntoPlay p "14009"
        haymaker <- putInHand p "01087"
        other <- putInHand p "01087"
        choose "play Haymaker" (isPlay haymaker)
        choose "pay with Friction Resistance" (isAbilityOfCard friction)
        choose "pay" (isPayWith other)
        chooseIf isFinishPayment
        chooseVillain
        villainDamageNow `shouldReturn` 3
        upgradeIsExhausted (upgradeIdOf friction) `shouldReturn` True
