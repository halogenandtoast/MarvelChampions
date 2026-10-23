module Marvel.Hero.SpiderWomanSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import TestImport

spiderWoman :: IO TestGame
spiderWoman = testGame ["spider-woman"]

-- | Spider-Woman, her hand empty.
asSpiderWoman :: IdentityId -> GameAppT ()
asSpiderWoman p = changeForm p >> pass >> clearHand p

agility :: Choice -> Bool
agility = isNamedAbility "\"Superhuman Agility\""

spec :: Spec
spec = describe "Spider-Woman" do
  describe "Jessica Drew's Double Agent (04031b)" do
    let deck = lookupStarterDeck "spider-woman"
    it "accepts her starter deck: Aggression and Justice, 11 cards of each" $
      (fmap aspectsBalanced deck `shouldBe` Just True :: IO ())
    it "refuses one aspect, or unequal numbers of the two (FAQ)" $
      for_ deck \d -> liftIO @IO do
        aspectsBalanced d {aspects = ["aggression"]} `shouldBe` False
        aspectsBalanced d {cards = (CardCode "04043", 1) : d.cards} `shouldBe` False
        -- her own aspect cards are not counted
        aspectsBalanced d {cards = (CardCode "04035", 1) : d.cards} `shouldBe` True
    it "asks nothing of other heroes" $
      (fmap aspectsBalanced (lookupStarterDeck "hawkeye") `shouldBe` Just True :: IO ())

  describe "Superhuman Agility (04031a)" do
    it "gives +1 THW, ATK and DEF until the end of the round as she plays an aspect card" do
      tg <- spiderWoman
      let p = player1 tg
      inGame tg do
        asSpiderWoman p
        strike <- putInHand p "04044"
        payment <- traverse (putInHand p) ["01090", "01090"]
        playWith strike payment
        choose "Superhuman Agility" agility
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 3
        -- her basic attack: 1 ATK, +1
        choose "attack" (isHeroAttack p)
        choose "the target" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 5
        resolve [EndRound]
        null <$> getsGame gameEffects `shouldReturn` True

    it "is limited to once a round for each aspect" do
      tg <- spiderWoman
      let p = player1 tg
      inGame tg do
        asSpiderWoman p
        strike <- putInHand p "04044"
        payment <- traverse (putInHand p) ["01090", "01090"]
        playWith strike payment
        choose "Superhuman Agility" agility
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        pass
        -- another Aggression card: the Aggression use is spent
        blast <- putInHand p "04035"
        energy <- putInHand p "01088"
        playWith blast [energy]
        offers agility `shouldReturn` False
        choose "the target" (isTargetChoice vid)
        pass
        -- a Justice card has its own
        placeMainThreat 3
        area <- putInHand p "04049"
        genius <- putInHand p "01089"
        playWith area [genius]
        offers agility `shouldReturn` True

  describe "The Viper (04054)" do
    it "reduces the hand size of the player she is engaged with by 1" do
      tg <- spiderWoman
      let p = player1 tg
      inGame tg do
        before <- handSizeOf p
        _ <- addMinion p Minions.theViper
        pass
        handSizeOf p `shouldReturn` before - 1

  describe "nemesis set" do
    it "is set aside at setup, Uncertain Loyalties shuffled into the encounter deck" do
      tg <- spiderWoman
      inGame tg do
        aside <- getsGame (map toCardCode . (.scenarioSetAsideCards) . toAttrs . gameScenario)
        for_ ["04054", "04055", "04056", "04057"] \c -> aside `shouldSatisfy` elem c
        map toCardCode <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` elem "04053")
