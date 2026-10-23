module Marvel.Hero.HawkeyeSpec (spec) where

import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types (villainConfused)
import TestImport

hawkeye :: IO TestGame
hawkeye = testGame ["hawkeye"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | Hawkeye, his hand empty, his bow in play.
withBow :: IdentityId -> GameAppT PlayerCard
withBow p = do
  changeForm p
  pass
  clearHand p
  putIntoPlay p "04002"

held :: GameAppT [PlayerCard]
held = concatMap (.upgradeHeldCards) <$> upgradesWith "04003"

spec :: Spec
spec = describe "Hawkeye" do
  describe "Hawkeye's Quiver (04003)" do
    it "searches the top 5 cards for an Arrow event, holds it faceup, and shuffles the rest back (FAQ)" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        _ <- withBow p
        quiver <- putIntoPlay p "04003"
        deckBefore <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs p
        [_, sonic, _] <- stackDeck p ["01088", "04005", "01089"]
        choose "use the quiver" (isAbilityOfCard quiver)
        choose "take Sonic Arrow" (isTargetChoice sonic)
        map pcCardId <$> held `shouldReturn` [pcCardId sonic]
        length . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` deckBefore + 2
        upgradesWith "04003" >>= \us -> map (.upgradeExhausted) us `shouldBe` [True]

    it "plays its Arrow events as if in hand, leaving the quiver" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        _ <- withBow p
        quiver <- putIntoPlay p "04003"
        [sonic] <- stackDeck p ["04005"]
        choose "use the quiver" (isAbilityOfCard quiver)
        choose "take Sonic Arrow" (isTargetChoice sonic)
        payment <- traverse (putInHand p) ["01089", "01089"]
        playWith sonic payment
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        pass
        held `shouldReturn` []
        villainDamageNow `shouldReturn` 3
        villainConfused . toAttrs <$> getVillain `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem sonic)

  describe "exhausting Hawkeye's Bow as an Arrow event's cost" do
    it "is paid as the event is played" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        bow <- withBow p
        vibranium <- putInHand p "04009"
        playPaying p vibranium ["01088", "01088"]
        chooseVillain
        pass
        upgradeIsExhausted (upgradeOf bow) `shouldReturn` True
        villainDamageNow `shouldReturn` 6

    it "needs the bow in play and ready" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        vibranium <- putInHand p "04009"
        _ <- traverse (putInHand p) ["01088", "01088"]
        offers (isPlay vibranium) `shouldReturn` False
        bow <- putIntoPlay p "04002"
        offers (isPlay vibranium) `shouldReturn` True
        resolve [UpgradeMessage (upgradeOf bow) ExhaustedUpgrade]
        offers (isPlay vibranium) `shouldReturn` False

    it "Quick Draw readies the bow, exhausting Hawkeye" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        bow <- withBow p
        resolve [UpgradeMessage (upgradeOf bow) ExhaustedUpgrade]
        choose "quick draw" (isNamedAbility "\"Quick Draw\"")
        upgradeIsExhausted (upgradeOf bow) `shouldReturn` False
        exhaustedNow p `shouldReturn` True

  describe "Mockingbird (04004)" do
    it "returns to hand to prevent all damage from the villain's attack" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        _ <- withBow p
        bird <- putIntoPlay p "04004"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "Mockingbird" (isAbilityOfCard bird)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        damageOf p `shouldReturn` 0
        map pcCardId <$> handOf p `shouldReturn` [pcCardId bird]

  describe "nemesis set" do
    it "is set aside at setup, Criminal Past shuffled into the encounter deck" do
      tg <- hawkeye
      inGame tg do
        aside <- getsGame (map toCardCode . scenarioSetAsideCards' . gameScenario)
        for_ ["04027", "04028", "04029", "04030"] \c -> aside `shouldSatisfy` elem c
        deck <- map toCardCode <$> scenarioEncounterDeckNow
        deck `shouldSatisfy` elem "04026"

  describe "Crossfire (04027)" do
    it "attacks with piercing as he engages a hero (quickstrike)" do
      tg <- hawkeye
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        resolve [IdentityMessage p IdentityBecomeTough]
        _ <- addMinion p Minions.crossfire
        pass
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        damageOf p `shouldReturn` 2
 where
  scenarioSetAsideCards' = (.scenarioSetAsideCards) . toAttrs
