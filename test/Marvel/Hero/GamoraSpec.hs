module Marvel.Hero.GamoraSpec (spec) where

import TestImport

gamoraGame :: IO TestGame
gamoraGame = testGame ["gamora"]

-- | Gamora in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

isFinesse, isPrecision :: Choice -> Bool
isFinesse = isNamedAbility "Finesse"
isPrecision = isNamedAbility "Precision"

-- | The alter-ego's Action: her identity's ability 1, not changing form.
isLookAction :: IdentityId -> Choice -> Bool
isLookAction p = \case
  UseAbility ab -> runAbility p 1 `elem` abilityChoices ab
  _ -> False

-- | Play an event from hand paying with an Energy (2 resources): Haymaker (01087, an
-- Attack event) on the villain, For Justice! (01060, a Thwart event) on the main scheme.
playHaymaker, playForJustice :: IdentityId -> GameAppT ()
playHaymaker p = do
  card <- putInHand p "01087"
  energy <- putInHand p "01088"
  playWith card [energy]
  vid <- villainIdNow
  chooseIf (isTargetChoice vid)
playForJustice p = do
  card <- putInHand p "01060"
  energy <- putInHand p "01088"
  playWith card [energy]
  ms <- mainSchemeIdNow
  chooseIf (isTargetChoice ms)

chooseVillainIf :: GameAppT ()
chooseVillainIf = villainIdNow >>= chooseIf . isTargetChoice

codesOf :: [PlayerCard] -> [CardCode]
codesOf = map (toCardCode . getCardDef)

spec :: Spec
spec = describe "Gamora" do
  it "deals the pack's own deck of 40 cards" do
    sum (map snd (starter "gamora").cards) `shouldBe` (40 :: Int) :: IO ()

  describe "Gamora (18001a)" do
    it "Finesse: after you play an Attack event, remove 1 threat from a scheme, once a phase" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        threat <- mainSchemeThreatNow
        playHaymaker p
        offers isPrecision `shouldReturn` False
        choose "Finesse" isFinesse
        mainSchemeIdNow >>= chooseIf . isTargetChoice
        mainSchemeThreatNow `shouldReturn` (threat - 1)
        playHaymaker p
        offers isFinesse `shouldReturn` False

    it "Precision: after you play a Thwart event, deal 1 damage to an enemy, once a phase" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        playForJustice p
        offers isFinesse `shouldReturn` False
        damage <- villainDamageNow
        choose "Precision" isPrecision
        chooseVillainIf
        villainDamageNow `shouldReturn` (damage + 1)
        placeMainThreat 3
        playForJustice p
        offers isPrecision `shouldReturn` False

    it "Finesse and Precision are limited apart: each is used once in the same phase" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        playHaymaker p
        choose "Finesse" isFinesse
        mainSchemeIdNow >>= chooseIf . isTargetChoice
        playForJustice p
        damage <- villainDamageNow
        choose "Precision" isPrecision
        chooseVillainIf
        villainDamageNow `shouldReturn` (damage + 1)

  describe "Gamora (18001b)" do
    it "looks at the top card of her deck and draws it when it is an Attack or Thwart event" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        clearHand p
        [top] <- stackDeck p ["01087"]
        choose "look at the top card" (isLookAction p)
        chooseIf (isLabelStarting "$label.drawCards")
        handOf p `shouldReturn` [top]
        offers (isLookAction p) `shouldReturn` False

    it "leaves any other card on top of her deck" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        clearHand p
        [top] <- stackDeck p ["01086"]
        choose "look at the top card" (isLookAction p)
        offers (isLabelStarting "$label.drawCards") `shouldReturn` False
        chooseIf (isLabel "$label.done")
        handOf p `shouldReturn` []
        headMay . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` Just top

    it "is not used as Gamora the hero" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        offers (isLookAction p) `shouldReturn` False

  describe "Nebula (18002)" do
    it "after she enters play, searches the deck for an Attack or Thwart event" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        clearHand p
        [haymaker, firstAid] <- stackDeck p ["01087", "01086"]
        nebula <- putInHand p "18002"
        energy <- putInHand p "01088"
        playWith nebula [energy]
        choose "use Nebula" (isAbilityOfCard nebula)
        offers (isTargetChoice firstAid) `shouldReturn` False
        choose "Haymaker" (isTargetChoice haymaker)
        handOf p `shouldReturn` [haymaker]

  describe "Conditioning Room (18008)" do
    it "returns the bottommost Attack or Thwart event from the discard pile and heals Gamora 1" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        clearHand p
        setDamage p 2
        haymaker <- putInDiscard p "01087"
        _ <- putInDiscard p "01060"
        _ <- putInDiscard p "01086"
        room <- putIntoPlay p "18008"
        choose "use Conditioning Room" (isAbilityOfCard room)
        handOf p `shouldReturn` [haymaker]
        damageOf p `shouldReturn` 1
        codesOf <$> discardOf p `shouldReturn` ["01086", "01060"]

    it "is not offered without an Attack or Thwart event in the discard pile, nor in hero form" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInDiscard p "01086"
        room <- putIntoPlay p "18008"
        offers (isAbilityOfCard room) `shouldReturn` False
        _ <- putInDiscard p "01087"
        offers (isAbilityOfCard room) `shouldReturn` True
        asHero p
        offers (isAbilityOfCard room) `shouldReturn` False

  describe "Keen Instincts (18009)" do
    it "generates a wild resource for an Attack or Thwart event" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        keen <- putIntoPlay p "18009"
        haymaker <- putInHand p "01087"
        firstAid <- putInHand p "01086"
        damage <- villainDamageNow
        choose "play Haymaker" (isPlay haymaker)
        choose "pay with Keen Instincts" (isAbilityOfCard keen)
        payWith [firstAid]
        chooseVillainIf
        upgradeIsExhausted (upgradeIdOf keen) `shouldReturn` True
        villainDamageNow `shouldReturn` (damage + 3)

    it "pays for no other card" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "18009"
        firstAid <- putInHand p "01086"
        offers (isPlay firstAid) `shouldReturn` False

  describe "Gamora's Sword (18010)" do
    it "after you play an Attack event, deals 1 damage to an enemy, each time" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        sword <- putIntoPlay p "18010"
        damage <- villainDamageNow
        playHaymaker p
        choose "Gamora's Sword" (isAbilityOfCard sword)
        chooseVillainIf
        villainDamageNow `shouldReturn` (damage + 4)
        pass
        playHaymaker p
        offers (isAbilityOfCard sword) `shouldReturn` True

    it "does not answer a Thwart event" do
      tg <- gamoraGame
      let p = player1 tg
      inGame tg do
        asHero p
        sword <- putIntoPlay p "18010"
        placeMainThreat 3
        playForJustice p
        offers (isAbilityOfCard sword) `shouldReturn` False
