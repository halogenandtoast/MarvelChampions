module Marvel.Hero.RocketRaccoonSignatureSpec (spec) where

import Marvel.Card.Side (Side (..))
import Marvel.Hero.Runner (getModifiedThwart)
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (ThwartModifier))
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

rocket :: IO TestGame
rocket = testGame ["rocket-raccoon"]

-- | Rocket Raccoon in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

heroThwart :: IdentityId -> GameAppT Natural
heroThwart p =
  getPlayer p >>= \i -> case currentIdentity i of
    HeroSide h -> getModifiedThwart (toAttrs h)
    AlterEgoSide _ -> error "Rocket Raccoon is in alter-ego form"

isDeclareWild :: Choice -> Bool
isDeclareWild = \case
  DeclareWild _ _ -> True
  _ -> False

-- | Play a card from hand paying with new copies of the cards given, declaring a wild
-- resource if asked.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card payment = do
  cards <- traverse (putInHand p) payment
  playWith card cards
  chooseIf isDeclareWild
  whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment

-- | End the turn and decline everything until the player's next turn. The encounter
-- deck is only Advance, so the villain phase stuns, confuses, toughens and guards no one.
toNextTurn :: (HasCallStack) => GameAppT ()
toNextTurn = do
  _ <- setEncounterDeck (replicate 6 Treacheries.advance) []
  endTurn
  go (40 :: Int)
 where
  go 0 = error "the next turn did not come"
  go n = do
    pass
    keepCards
    whenM (not <$> offers (== EndTurn)) do
      asked >>= \case
        Just (ident, _) -> answerQuestion ident 0
        Nothing -> error "nothing asked"
      go (n - 1)

basicAttackVillain :: IdentityId -> GameAppT ()
basicAttackVillain p = do
  choose "attack" (isHeroAttack p)
  chooseVillain
  pass

deckTop :: IdentityId -> GameAppT (Maybe PlayerCard)
deckTop p = headMay . unDeck . (.playerIdentityDeck) <$> playerAttrs p

exhaustUpgrade :: PlayerCard -> GameAppT ()
exhaustUpgrade c = resolve [UpgradeMessage (upgradeIdOf c) ExhaustedUpgrade]

exhaustOption, discardOption :: Choice -> Bool
exhaustOption = isLabel "$label.exhaustRocketRaccoonRemoveCrisisOnHalfworld"
discardOption = isLabel "$label.discardHighestCostUpgradeDiscardObligation"

spec :: Spec
spec = describe "Rocket Raccoon signature cards" do
  describe "I've Got a Plan (16030)" do
    it "after a basic thwart, readies Rocket Raccoon and gives him +1 THW" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        plan <- putInHand p "16030"
        genius <- putInHand p "01089"
        choose "thwart" (isHeroThwart p)
        chooseMainScheme
        isExhaustedNow p `shouldReturn` True
        playWith plan [genius]
        pass
        isExhaustedNow p `shouldReturn` False
        heroThwart p `shouldReturn` 3

    it "is not offered after a thwart event" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        plan <- putInHand p "16030"
        forJustice <- putInHand p "01060"
        playPaying p forJustice ["01088"]
        chooseMainScheme
        offers (isPlay plan) `shouldReturn` False

    it "is not offered outside its window" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        plan <- putInHand p "16030"
        _ <- putInHand p "01089"
        offers (isPlay plan) `shouldReturn` False

    it "the +1 THW is only while in hero form" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        plan <- putInHand p "16030"
        genius <- putInHand p "01089"
        choose "thwart" (isHeroThwart p)
        chooseMainScheme
        playWith plan [genius]
        pass
        resolve [IdentityMessage p (ChangedToForm B)]
        getModifiers p >>= (`shouldSatisfy` notElem (ThwartModifier 1))
        resolve [IdentityMessage p (ChangedToForm A)]
        heroThwart p `shouldReturn` 3

    it "the +1 THW ends with the phase" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        plan <- putInHand p "16030"
        genius <- putInHand p "01089"
        choose "thwart" (isHeroThwart p)
        chooseMainScheme
        playWith plan [genius]
        pass
        toNextTurn
        heroThwart p `shouldReturn` 2

  describe "Reload (16031)" do
    it "readies each Tech upgrade you control, no other upgrade" do
      tg <- testGame ["rocket-raccoon", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        pistol <- putIntoPlay p "16038"
        battery <- putIntoPlay p "16034"
        follow <- putIntoPlay p "16045"
        shooter <- putIntoPlay other "01008"
        traverse_ exhaustUpgrade [pistol, battery, follow, shooter]
        asHero p
        reload <- putInHand p "16031"
        playPaying p reload ["01088"]
        pass
        upgradeIsExhausted (upgradeIdOf pistol) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf battery) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf follow) `shouldReturn` True
        upgradeIsExhausted (upgradeIdOf shooter) `shouldReturn` True

  describe "Schadenfreude (16032)" do
    it "heals 2 each time you deal damage to an enemy this turn" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 6
        minion <- addMinion p Minions.hydraMercenary
        card <- putInHand p "16032"
        playPaying p card ["01090"]
        pass
        haymaker <- putInHand p "01087"
        playPaying p haymaker ["01088"]
        choose "the minion" (isTargetChoice minion)
        pass
        damageOf p `shouldReturn` 4
        basicAttackVillain p
        damageOf p `shouldReturn` 2

    it "does not heal for an ally's damage" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01083"
        bird <- allyIn "01083"
        setDamage p 6
        card <- putInHand p "16032"
        playPaying p card ["01090"]
        pass
        before <- villainDamageNow
        choose "Mockingbird attacks" (isAllyAttack bird)
        chooseVillain
        pass
        villainDamageNow >>= (`shouldSatisfy` (> before))
        damageOf p `shouldReturn` 6

    it "heals when tough prevents the damage: prevented, it was still dealt" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 6
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        card <- putInHand p "16032"
        playPaying p card ["01090"]
        pass
        basicAttackVillain p
        villainDamageNow `shouldReturn` 0
        damageOf p `shouldReturn` 4

    it "ends with the turn" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16032"
        playPaying p card ["01090"]
        pass
        toNextTurn
        setDamage p 6
        basicAttackVillain p
        damageOf p `shouldReturn` 6

  describe "Salvage (16033)" do
    it "after it is spent, puts a Tech upgrade from your discard on top of your deck" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        battery <- putInDiscard p "16034"
        follow <- putInDiscard p "16045"
        reload <- putInHand p "16031"
        playPaying p reload ["16033"]
        offers (isTargetChoice follow) `shouldReturn` False
        choose "Battery Pack" (isTargetChoice battery)
        pass
        deckTop p `shouldReturn` Just battery
        discardOf p >>= (`shouldSatisfy` notElem battery)

    it "may take back a Tech upgrade spent with it" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        schadenfreude <- putInHand p "16032"
        salvage <- putInHand p "16033"
        battery <- putInHand p "16034"
        playWith schadenfreude [salvage, battery]
        chooseIf isDeclareWild
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        choose "Battery Pack" (isTargetChoice battery)
        pass
        deckTop p `shouldReturn` Just battery

    it "may be declined" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        battery <- putInDiscard p "16034"
        reload <- putInHand p "16031"
        playPaying p reload ["16033"]
        choose "decline" isNoResponse
        pass
        discardOf p >>= (`shouldSatisfy` elem battery)

    it "asks nothing with no Tech upgrade in your discard" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putInDiscard p "16045"
        top <- deckTop p
        reload <- putInHand p "16031"
        playPaying p reload ["16033"]
        offers isNoResponse `shouldReturn` False
        deckTop p `shouldReturn` top

  describe "Crisis on Halfworld (16053)" do
    it "is given to the Rocket Raccoon player" do
      tg <- testGame ["spider-man", "rocket-raccoon"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.crisisOnHalfworld
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting the alter-ego removes it from the game" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.crisisOnHalfworld
        offers discardOption `shouldReturn` True
        choose "exhaust Rocket" exhaustOption
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "16053")

    it "a hero may flip to alter-ego to exhaust it" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- revealNow p Obligations.crisisOnHalfworld
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        choose "exhaust Rocket" exhaustOption
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "16053")

    it "an exhausted alter-ego cannot be exhausted for it" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.crisisOnHalfworld
        offers exhaustOption `shouldReturn` False

    it "otherwise discards the highest cost upgrade you control, ties your choice" do
      tg <- testGame ["rocket-raccoon", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        battery <- putIntoPlay p "16034"
        cannon <- putIntoPlay p "16036"
        launcher <- putIntoPlay p "16037"
        armor <- putIntoPlay other "01036"
        changeForm p >> pass
        _ <- revealNow p Obligations.crisisOnHalfworld
        choose "stay" (isLabel "$label.doNotFlipToAlterEgo")
        offers (isTargetChoice (upgradeIdOf battery)) `shouldReturn` False
        offers (isTargetChoice (upgradeIdOf armor)) `shouldReturn` False
        offers (isTargetChoice (upgradeIdOf cannon)) `shouldReturn` True
        choose "Rocket Launcher" (isTargetChoice (upgradeIdOf launcher))
        pass
        ups <- upgradesOf p
        ups `shouldMatchList` [upgradeIdOf battery, upgradeIdOf cannon]
        upgradesOf other `shouldReturn` [upgradeIdOf armor]
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "16053")

    it "surges when no upgrade is discarded" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.crisisOnHalfworld
        choose "stay" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)
        obligationsInPlay `shouldReturn` 0
