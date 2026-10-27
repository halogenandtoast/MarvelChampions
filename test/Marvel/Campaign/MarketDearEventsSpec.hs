module Marvel.Campaign.MarketDearEventsSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Hawkeye, in play for the player and exhausted.
exhaustedHawkeye :: IdentityId -> GameAppT (Attrs Ally)
exhaustedHawkeye p = do
  _ <- putIntoPlay p "01066"
  hawkeye <- allyIn "01066"
  resolve [AllyMessage (allyId hawkeye) ExhaustedAlly]
  pure hawkeye

hawkeyeExhausted :: GameAppT Bool
hawkeyeExhausted = (.allyExhausted) <$> allyIn "01066"

-- | The card is nowhere a player card could be: removed from the game.
isRemoved :: IdentityId -> PlayerCard -> GameAppT ()
isRemoved p card = do
  discardOf p >>= (liftIO . (`shouldNotContain` [card]))
  handOf p >>= (liftIO . (`shouldNotContain` [card]))

spec :: Spec
spec = describe "The Market's dear events" do
  describe "Ardent Resolve (16166)" do
    it "readies a friendly character, then draws 1 card" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        hawkeye <- exhaustedHawkeye p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "16166"
        playWith card []
        -- the exhausted hero is offered too
        offers (isTargetChoice p) `shouldReturn` True
        choose "Hawkeye" (isTargetChoice (allyId hawkeye))
        hawkeyeExhausted `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 1
        discardOf p >>= (liftIO . (`shouldContain` [card]))

    it "draws with no exhausted character to ready" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16166"
        playWith card []
        length <$> handOf p `shouldReturn` 1

  describe "Onrush (16167)" do
    it "cancels a treachery revealed from the encounter deck and discards it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        threat <- mainSchemeThreatNow
        card <- putInHand p "16167"
        advance <- encounterCard Treacheries.advance
        runNow [RevealEncounterCard p advance]
        playWith card []
        mainSchemeThreatNow `shouldReturn` threat
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId advance))

    it "cancels a minion: it does not enter play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16167"
        regular <- encounterCard Minions.hydraRegular
        runNow [RevealEncounterCard p regular]
        playWith card []
        minionDamages `shouldReturn` []
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId regular))

    it "is a hero interrupt" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "16167"
        regular <- encounterCard Minions.hydraRegular
        runNow [RevealEncounterCard p regular]
        offers (isPlay card) `shouldReturn` False

  describe "Safeguard (16168)" do
    it "gives up to 2 friendly characters a tough status card, then draws 1 card" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        hawkeye <- exhaustedHawkeye p
        card <- putInHand p "16168"
        playWith card []
        choose "Spider-Man" (isTargetChoice p)
        choose "Hawkeye" (isTargetChoice (allyId hawkeye))
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        (.allyTough) <$> allyIn "01066" `shouldReturn` True
        length <$> handOf p `shouldReturn` 1

    it "may stop at one, and does not offer a character already tough" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        hawkeye <- exhaustedHawkeye p
        resolve [IdentityMessage p IdentityBecomeTough]
        card <- putInHand p "16168"
        playWith card []
        offers (isTargetChoice p) `shouldReturn` False
        choose "Hawkeye" (isTargetChoice (allyId hawkeye))
        (.allyTough) <$> allyIn "01066" `shouldReturn` True
        length <$> handOf p `shouldReturn` 1

  describe "Sure Gamble (16169)" do
    it "reduces the cost of the next card played this phase by 3, and only the next" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        haymaker <- putInHand p "01087"
        offers (isPlay haymaker) `shouldReturn` False
        card <- putInHand p "16169"
        playWith card []
        -- Haymaker's 2 is now 0
        playWith haymaker []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        haymaker2 <- putInHand p "01087"
        offers (isPlay haymaker2) `shouldReturn` False

  describe "Grand Strategy (16174)" do
    it "draws up to the maximum hand size and is removed from the game" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16174"
        playWith card []
        size <- handSizeOf p
        length <$> handOf p `shouldReturn` fromIntegral size
        isRemoved p card

  describe "Power Unleashed (16175)" do
    it "deals 5 damage to the villain, removes 5 threat from the main scheme, and is removed from the game" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        card <- putInHand p "16175"
        playWith card []
        pass
        villainDamageNow `shouldReturn` 5
        mainSchemeThreatNow `shouldReturn` (threat - 5)
        isRemoved p card

  describe "Tried and True (16176)" do
    it "lets the chosen player add up to 3 cards from their discard pile to hand, and is removed from the game" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        [a, b, c, d] <- stackDiscard p ["01087", "01088", "01089", "01090"]
        card <- putInHand p "16176"
        playWith card []
        -- the only player, so not asked
        for_ [a, b, c] \x -> choose "a card" (isTargetChoice x)
        handOf p >>= (`shouldMatchList` [a, b, c])
        discardOf p >>= (`shouldMatchList` [d])
        isRemoved p card

    it "may take fewer, and lets another player take from their own discard pile" do
      tg <- testGame ["spider-man", "she-hulk"]
      let p = player1 tg
          p2 = player2 tg
      inGame tg do
        asHero p
        [a, _] <- stackDiscard p2 ["01087", "01088"]
        card <- putInHand p "16176"
        playWith card []
        -- Spider-Man's discard pile is empty: She-Hulk is the only player to choose,
        -- and she picks from her own discard pile
        choose "a card" (isTargetChoice a)
        choose "done" (isLabel "$label.done")
        handOf p2 >>= (liftIO . (`shouldContain` [a]))

  describe "Triple Threat (16177)" do
    it "readies up to 3 characters and is removed from the game" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        hawkeye <- exhaustedHawkeye p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "16177"
        playWith card []
        choose "Spider-Man" (isTargetChoice p)
        choose "Hawkeye" (isTargetChoice (allyId hawkeye))
        exhaustedNow p `shouldReturn` False
        hawkeyeExhausted `shouldReturn` False
        isRemoved p card

    it "may stop after one" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- exhaustedHawkeye p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "16177"
        playWith card []
        choose "Spider-Man" (isTargetChoice p)
        choose "done" (isLabel "$label.done")
        exhaustedNow p `shouldReturn` False
        hawkeyeExhausted `shouldReturn` True

    it "is not played with no exhausted character" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16177"
        offers (isPlay card) `shouldReturn` False
