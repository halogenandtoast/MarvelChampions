module Marvel.Hero.GrootSignatureSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

groot :: IO TestGame
groot = testGame ["groot"]

-- | Groot in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

growth :: IdentityId -> GameAppT Natural
growth p = (.playerIdentityCounters) <$> playerAttrs p

setGrowth :: IdentityId -> Natural -> GameAppT ()
setGrowth p n = resolve [RemoveCounters (toTarget p) 10, PlaceCounters (toTarget p) n]

isTough :: IdentityId -> GameAppT Bool
isTough p = (.playerIdentityTough) <$> playerAttrs p

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | Play the card, paying with one basic resource per cost.
playPaying :: IdentityId -> CardCode -> Int -> GameAppT ()
playPaying p code cost = do
  card <- putInHand p code
  payment <- traverse (putInHand p) (take cost $ cycle ["01088", "01089", "01090"])
  playWith card payment

removeGrowth :: Natural -> Choice -> Bool
removeGrowth n = isLabel (countLabel "removeGrowthCounters" n)

exhaustAlterEgo :: Choice -> Bool
exhaustAlterEgo = isLabel "$label.exhaustGrootRemoveWilt"

spec :: Spec
spec = describe "Groot signature" do
  describe "Fruition (16002)" do
    it "places 2 growth counters on Groot, even in alter-ego form" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        clearHand p
        setGrowth p 1
        playPaying p "16002" 0
        pass
        growth p `shouldReturn` 3

    it "places none beyond 10" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        clearHand p
        setGrowth p 9
        playPaying p "16002" 0
        pass
        growth p `shouldReturn` 10

  describe "\"I am Groot\" (16003)" do
    it "removes threat equal to Groot's growth counters" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        setGrowth p 4
        playPaying p "16003" 3
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        pass
        mainSchemeThreatNow `shouldReturn` (before - 4)
        growth p `shouldReturn` 4

    it "is a hero action" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        card <- putInHand p "16003"
        traverse_ (putInHand p) ["01088", "01089", "01090"]
        offers (isPlay card) `shouldReturn` False

  describe "\"I. AM. GROOT!\" (16004)" do
    it "deals damage to an enemy equal to Groot's growth counters" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        setGrowth p 5
        playPaying p "16004" 2
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        growth p `shouldReturn` 5

  describe "Root Stomp (16005)" do
    it "deals 5 damage, and places a growth counter when it defeats the enemy" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        setGrowth p 2
        mercenary <- addMinion p Minions.hydraMercenary
        playPaying p "16005" 2
        choose "the mercenary" (isTargetChoice (EnemyMinionId mercenary))
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        growth p `shouldReturn` 3

    it "places no counter when the enemy survives" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        setGrowth p 2
        playPaying p "16005" 2
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        growth p `shouldReturn` 2

    it "places none beyond 10" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        setGrowth p 10
        mercenary <- addMinion p Minions.hydraMercenary
        playPaying p "16005" 2
        choose "the mercenary" (isTargetChoice (EnemyMinionId mercenary))
        pass
        growth p `shouldReturn` 10

  describe "\"We Are Groot\" (16006)" do
    it "removes the counters chosen and gives that many different friendly characters a tough status" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01075"
        setGrowth p 3
        playPaying p "16006" 1
        offers (removeGrowth 3) `shouldReturn` False -- two friendly characters
        choose "remove 2" (removeGrowth 2)
        growth p `shouldReturn` 1
        choose "Groot" (isTargetChoice p)
        widow <- allyIn "01075"
        offers (isTargetChoice p) `shouldReturn` False
        choose "the ally" (isTargetChoice (allyId widow))
        pass
        isTough p `shouldReturn` True
        (.allyTough) <$> allyIn "01075" `shouldReturn` True

    it "removes at most 4, and at most the counters he has" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        traverse_ (putIntoPlay p) ["01075", "01076", "01067"] -- Groot and three allies
        setGrowth p 6
        playPaying p "16006" 1
        offers (removeGrowth 4) `shouldReturn` True
        offers (removeGrowth 5) `shouldReturn` False

    it "removes only 1 when Groot has only 1" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01075"
        setGrowth p 1
        playPaying p "16006" 1
        offers (removeGrowth 2) `shouldReturn` False
        choose "remove 1" (removeGrowth 1)
        choose "Groot" (isTargetChoice p)
        pass
        growth p `shouldReturn` 0
        isTough p `shouldReturn` True
        (.allyTough) <$> allyIn "01075" `shouldReturn` False

  describe "Wilt (16025)" do
    it "is given to the Groot player" do
      tg <- testGame ["she-hulk", "groot"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.wilt
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting the alter-ego removes it from the game" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        setGrowth p 4
        _ <- revealNow p Obligations.wilt
        offers (removeGrowth 3) `shouldReturn` True
        choose "exhaust Groot" exhaustAlterEgo
        exhaustedNow p `shouldReturn` True
        growth p `shouldReturn` 4
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "16025")

    it "a hero may flip to alter-ego to exhaust it" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.wilt
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Groot" exhaustAlterEgo
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "16025")

    it "an exhausted alter-ego cannot be exhausted for it" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        setGrowth p 4
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.wilt
        offers exhaustAlterEgo `shouldReturn` False
        pass
        growth p `shouldReturn` 1

    it "otherwise removes 3 growth counters and is discarded, without surge" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        setGrowth p 5
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.wilt
        choose "remove counters" (removeGrowth 3)
        pass
        growth p `shouldReturn` 2
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "16025")
        map ecCardId <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` elem (ecCardId next))

    it "removes as many as he has, short of 3, without surge" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        setGrowth p 2
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.wilt
        choose "remove counters" (removeGrowth 3)
        pass
        growth p `shouldReturn` 0
        map ecCardId <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` elem (ecCardId next))

    it "gains surge when no counters were removed" do
      tg <- groot
      let p = player1 tg
      inGame tg do
        setGrowth p 0
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.wilt
        choose "remove counters" (removeGrowth 3)
        pass
        encounterDiscardCodes >>= (`shouldSatisfy` elem "16025")
        map ecCardId <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` notElem (ecCardId next))
