module Marvel.Hero.StarLordSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Side (Side (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import TestImport

import Control.Monad (replicateM, replicateM_)

starLord :: IO TestGame
starLord = testGame ["star-lord"]

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | Star-Lord in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Play a card from hand paying with new Energy cards (2 resources each), declining
-- an interrupt to its being played.
playPaying :: IdentityId -> PlayerCard -> Int -> GameAppT ()
playPaying p card n = do
  payment <- replicateM n (putInHand p "01088")
  choose "play the card" (isPlay card)
  chooseIf isNoResponse
  payWith payment

-- | Deal the player facedown encounter cards.
dealFacedown :: IdentityId -> Int -> GameAppT ()
dealFacedown p n = runNow (replicate n $ DealEncounterCard p)

minionsInPlay :: GameAppT [MinionId]
minionsInPlay = getsGame (HashMap.keys . gameMinions)

exhaustQuill, discardGun :: Choice -> Bool
exhaustQuill = isLabel "$label.exhaustPeterQuillRemoveBanishment"
discardGun = isLabel "$label.discardElementGunOrThreat"

spec :: Spec
spec = describe "Star-Lord signature cards" do
  describe "Nova Prime (17002)" do
    it "played from hand, defeats a non-Elite minion" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        clearHand p
        merc <- addMinion p Minions.hydraMercenary
        sandman <- addMinion p Minions.sandman
        nova <- putInHand p "17002"
        playPaying p nova 3
        choose "Nova Prime's response" (isAbilityOfCard nova)
        offers (isTargetChoice sandman) `shouldReturn` False
        choose "the Hydra Mercenary" (isTargetChoice merc)
        minionDamageOf merc `shouldReturn` Nothing
        minionDamageOf sandman >>= (`shouldSatisfy` isJust)

    it "is not offered with only Elite minions in play" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        clearHand p
        sandman <- addMinion p Minions.sandman
        nova <- putInHand p "17002"
        playPaying p nova 3
        offers isEndTurn `shouldReturn` True
        minionDamageOf sandman >>= (`shouldSatisfy` isJust)

  describe "Daring Escape (17003)" do
    it "deals you a facedown encounter card to ready your hero and draw 1; the card is revealed in the villain phase" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        asHero p
        [merc] <- stackEncounterDeck [Minions.hydraMercenary]
        resolve [IdentityMessage p ExhaustedIdentity]
        escape <- putInHand p "17003"
        playWith escape []
        isExhaustedNow p `shouldReturn` False
        hand <- handOf p
        length hand `shouldBe` 1
        hand `shouldSatisfy` notElem escape
        map ecCardId <$> dealtTo p `shouldReturn` [ecCardId merc]
        minionsInPlay >>= (`shouldSatisfy` notElem (minionOf merc))
        runNow [RevealDealtEncounterCards]
        pass
        null <$> dealtTo p `shouldReturn` True
        minionsInPlay >>= (`shouldSatisfy` elem (minionOf merc))

  describe "Gutsy Move (17004)" do
    it "removes 2 threat with no facedown encounter card in front of you" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        before <- mainSchemeThreatNow
        move <- putInHand p "17004"
        playPaying p move 1
        m <- mainSchemeIdNow
        chooseIf (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` (before - 2)

    it "removes 2 more for each facedown encounter card in front of you" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        dealFacedown p 1
        length <$> dealtTo p `shouldReturn` 1
        before <- mainSchemeThreatNow
        move <- putInHand p "17004"
        playPaying p move 1
        m <- mainSchemeIdNow
        chooseIf (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` (before - 4)

  describe "Sliding Shot (17005)" do
    it "cannot be played without an Element Gun" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        asHero p
        shot <- putInHand p "17005"
        _ <- replicateM 2 (putInHand p "01088")
        offers (isPlay shot) `shouldReturn` False

    it "deals 5 damage, 2 more for each facedown encounter card in front of you" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "17007"
        dealFacedown p 1
        before <- villainDamageNow
        shot <- putInHand p "17005"
        playPaying p shot 2
        vid <- villainIdNow
        chooseIf (isTargetChoice (EnemyVillainId vid))
        villainDamageNow `shouldReturn` (before + 7)

  describe "Banishment (17024)" do
    it "is given to the Peter Quill player" do
      tg <- testGame ["thor", "star-lord"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.banishment
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting Peter Quill removes it from the game" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.banishment
        offers discardGun `shouldReturn` True
        choose "exhaust Peter Quill" exhaustQuill
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "17024")

    it "a hero may flip to alter-ego to exhaust Peter Quill" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- revealNow p Obligations.banishment
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Peter Quill" exhaustQuill
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "17024")

    it "otherwise discards an Element Gun from play, and is discarded" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        gun <- putIntoPlay p "17007"
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.banishment
        choose "discard an Element Gun" discardGun
        choose "the Element Gun" (isTargetChoice (upgradeIdOf gun))
        pass
        null <$> upgradesWith "17007" `shouldReturn` True
        isExhaustedNow p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before
        encounterDiscardCodes >>= (`shouldSatisfy` elem "17024")
        obligationsInPlay `shouldReturn` 0

    it "may discard another player's Element Gun" do
      tg <- testGame ["thor", "star-lord"]
      let p1 = player1 tg
      inGame tg do
        gun <- putIntoPlay p1 "17007"
        _ <- revealNow p1 Obligations.banishment
        choose "discard an Element Gun" discardGun
        choose "Thor's Element Gun" (isTargetChoice (upgradeIdOf gun))
        pass
        null <$> upgradesWith "17007" `shouldReturn` True

    it "with no Element Gun in play places 3 threat on the main scheme; a hero cannot exhaust Peter Quill" do
      tg <- starLord
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.banishment
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        -- Star-Lord is not Peter Quill: the other option resolves
        pass
        sideOf p `shouldReturn` A
        isExhaustedNow p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` (before + 3)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "17024")
        obligationsInPlay `shouldReturn` 0
