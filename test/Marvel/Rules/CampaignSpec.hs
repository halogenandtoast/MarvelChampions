module Marvel.Rules.CampaignSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Area (Pile (..), experimentalWeaponsDeck)
import Marvel.Campaign
import Marvel.Game.Public (publicGameFor)
import Marvel.Hp (unHp)
import Marvel.SideScheme.Types (Attrs (sideSchemeHeldCards))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Obligation.Types (Attrs (obligationOwner))
import TestImport

spiderMan :: CardCode
spiderMan = "01001a"

-- | A Rise of Red Skull campaign for Spider-Man with so many scenarios won, the next
-- played against Rhino (the box's own scenarios are not the test's).
campaignAt :: CampaignMode -> Int -> Campaign
campaignAt mode n = c {scenarios = [if i == n then "01094" else code | (i, code) <- zip [0 ..] c.scenarios], completed = n}
 where
  c = newCampaign mode [spiderMan]

rhino :: ScenarioSetup
rhino = fromMaybe (error "no Rhino") (recommendedSetup "01094")

-- | The campaign's next game, dealt and waiting on its first question.
dealCampaign :: Campaign -> [DeckList] -> IO TestGame
dealCampaign c decks = do
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) rhino decks 1
  seats <- runGameApp app (getsGame gameSeats)
  pure $ TestGame app ran logged seats

-- | The campaign's next game against the box's own scenario at that place.
dealScenario :: Campaign -> [DeckList] -> IO TestGame
dealScenario c decks = do
  let code = fromMaybe (error "campaign over") (currentScenario c)
      setup = fromMaybe (error "no such scenario") (recommendedSetup code)
  (app, ran, logged) <- dealtGameWith (campaignL ?~ c) setup decks 1
  seats <- runGameApp app (getsGame gameSeats)
  pure $ TestGame app ran logged seats

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "no campaign") . gameCampaign)

spiderManNow :: GameAppT CampaignPlayer
spiderManNow = fromMaybe (error "no Spider-Man in the log") . campaignPlayer spiderMan <$> campaignNow

isCard :: CardCode -> Choice -> Bool
isCard code = \case
  CardLabel c _ -> c == code
  _ -> False

withCard :: CardCode -> DeckList -> DeckList
withCard code d = d {cards = d.cards <> [(code, 1)]}

spec :: Spec
spec = describe "The Rise of Red Skull campaign" do
  it "writes a win in the campaign log: Crossbones leaves a Tech upgrade to choose" do
    tg <- dealCampaign (campaignAt StandardCampaign 0) [starter "spider-man"]
    inGame tg do
      toFirstTurn
      resolve [GameOver Won]
      getsGame gameState `shouldReturn` Finished Won
      (.completed) <$> campaignNow `shouldReturn` 1
      (.pending) <$> spiderManNow `shouldReturn` [ChooseTechUpgrade]

  it "records an expert campaign's remaining hit points with a win" do
    tg <- dealCampaign (campaignAt ExpertCampaign 0) [starter "spider-man"]
    inGame tg do
      toFirstTurn
      let p = player1 tg
      hp <- hitPointsOf p
      setDamage p 3
      resolve [GameOver Won]
      (.hitPoints) <$> spiderManNow `shouldReturn` Just (hp - 3)

  it "asks the choice a win left as the next game is set up, the card earned in play from the start" do
    let c = recordInLog (RecordChoice spiderMan ChooseTechUpgrade) (campaignAt StandardCampaign 1)
    tg <- dealCampaign c [starter "spider-man"]
    inGame tg do
      choose "choose Tactical Scanner" (isCard "04156")
      toFirstTurn
      upgradeCodes >>= (`shouldSatisfy` elem "04156")
      p <- spiderManNow
      p.techUpgrade `shouldBe` Just "04156"
      p.pending `shouldBe` []

  it "flips the Basic upgrade to its Improved side when Zola leaves the choice" do
    let c =
          recordInLog (RecordChoice spiderMan MayImproveUpgrade) $
            recordInLog (RecordConditionUpgrade spiderMan "04159a") (campaignAt StandardCampaign 4)
    tg <- dealCampaign c [withCard "04159a" (starter "spider-man")]
    inGame tg do
      choose "improve it" (isCard "04159b")
      toFirstTurn
      codes <- upgradeCodes
      codes `shouldSatisfy` elem "04159b"
      codes `shouldSatisfy` notElem "04159a"
      (.conditionUpgrade) <$> spiderManNow `shouldReturn` Just "04159b"

  it "Absorbing Man won: the delay counters are recorded, and each player may choose a Basic upgrade" do
    tg <- dealScenario ((newCampaign StandardCampaign [spiderMan]) {completed = 1}) [starter "spider-man"]
    inGame tg do
      toFirstTurn
      mid <- mainSchemeId
      resolve [PlaceCounters (MainSchemeRef mid) 3]
      resolve [GameOver Won]
      (.campaignLog.delayCounters) <$> campaignNow `shouldReturn` Just 3
      (.pending) <$> spiderManNow `shouldReturn` [ChooseBasicUpgrade]

  it "Taskmaster won: the Captive allies a player rescued are added to their deck" do
    let c = (newCampaign StandardCampaign [spiderMan]) {completed = 2}
    tg <- dealScenario c [starter "spider-man"]
    inGame tg do
      toFirstTurn
      _ <- putInHand (player1 tg) "04098"
      resolve [GameOver Won]
      (.rescuedAllies) <$> spiderManNow `shouldReturn` ["04098"]

  it "Zola won with Hydra Prison in play: the allies beneath it are removed from the campaign" do
    let c = recordInLog (RecordConditionUpgrade spiderMan "04159a") $ (newCampaign StandardCampaign [spiderMan]) {completed = 3}
    tg <- dealScenario c [withCard "04159a" (starter "spider-man")]
    inGame tg do
      toFirstTurn
      prison <- getsGame (map (sideSchemeHeldCards . toAttrs) . filter ((== "04122") . toCardCode . getCardDef) . foldr (:) [] . gameSideSchemes)
      prison `shouldSatisfy` (not . null)
      resolve [GameOver Won]
      removed <- (.campaignLog.alliesRemoved) <$> campaignNow
      removed `shouldBe` map toCardCode (concat prison)
      (.pending) <$> spiderManNow `shouldReturn` []

  it "Crossbones won: each Experimental attachment that entered the game is recorded, and shuffled into the next encounter deck" do
    tg <- dealScenario (newCampaign StandardCampaign [spiderMan]) [starter "spider-man"]
    inGame tg do
      toFirstTurn
      Pile before _ <- getPile experimentalWeaponsDeck
      resolve [UsingDeck experimentalWeaponsDeck (DrawAndRevealEncounterCard (player1 tg))]
      Pile after _ <- getPile experimentalWeaponsDeck
      let entered = [toCardCode w | w <- before, w `notElem` after]
      length entered `shouldBe` 1
      resolve [GameOver Won]
      c <- campaignNow
      c.campaignLog.experimentalWeapons `shouldBe` entered
      -- the next scenario's setup finds the card by its code
      tg2 <- liftIO $ dealScenario c [starter "spider-man"]
      liftIO $ inGame tg2 do
        toFirstTurn
        deck <- scenarioEncounterDeckNow
        discard <- encounterDiscardNow
        attached <- getsGame (map (toCardCode . getCardDef) . foldr (:) [] . gameAttachments)
        any ((`elem` entered) . toCardCode) (deck <> discard) || any (`elem` entered) attached `shouldBe` True

  it "Absorbing Man won: the delay counters placed on None Shall Pass are recorded" do
    tg <- dealScenario ((newCampaign StandardCampaign [spiderMan]) {completed = 1}) [starter "spider-man"]
    inGame tg do
      toFirstTurn
      resolve [AccelerateMainScheme, AccelerateMainScheme]
      resolve [GameOver Won]
      (.campaignLog.delayCounters) <$> campaignNow `shouldReturn` Just 2

  it "places threat for Absorbing Man's delay counters as Red Skull is set up" do
    plain <- testGameAgainst "01094" ["spider-man"]
    base <- inGame plain mainSchemeThreatNow
    let c = recordInLog (RecordDelayCounters 3) (campaignAt StandardCampaign 4)
    tg <- dealCampaign c [starter "spider-man"]
    inGame tg do
      toFirstTurn
      mainSchemeThreatNow `shouldReturn` (base + 3)

  it "starts an expert campaign's scenario at the hit points recorded, unless an obligation heals" do
    let c = recordInLog (RecordHitPoints spiderMan 4) (campaignAt ExpertCampaign 1)
    kept <- dealCampaign c [starter "spider-man"]
    inGame kept do
      choose "keep the hit points" (isLabel "$label.keepHitPoints")
      toFirstTurn
      hp <- hitPointsOf (player1 kept)
      damageOf (player1 kept) `shouldReturn` (hp - 4)
    healed <- dealCampaign c [starter "spider-man"]
    inGame healed do
      choose "heal" (isLabel "$label.takeObligationToHeal")
      toFirstTurn
      damageOf (player1 healed) `shouldReturn` 0
      obligations <- (.obligations) <$> spiderManNow
      length obligations `shouldBe` 1
      obligations `shouldSatisfy` all (`elem` expertCampaignObligations)

  it "puts setup cards into play, and a permanent one cannot leave it" do
    tg <- dealCampaign (campaignAt StandardCampaign 2) [withCard "04161a" (starter "spider-man")]
    inGame tg do
      toFirstTurn
      let p = player1 tg
      upgradeCodes >>= (`shouldSatisfy` elem "04161a")
      a <- playerAttrs p
      hitPointsOf p `shouldReturn` (unHp a.playerIdentityHP + 3)
      map toCardCode (unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck) `shouldSatisfy` notElem "04161a"
      uids <- upgradesOf p
      resolve (map (RemoveFromPlay . UpgradeRef) uids)
      upgradeCodes >>= (`shouldSatisfy` elem "04161a")

  it "shows the board which campaign and which of its scenarios the game is" do
    tg <- dealCampaign ((campaignAt ExpertCampaign 1) {campaignName = Just "Hydra Rising"}) [starter "spider-man"]
    view <- publicGameFor (const True) =<< inGame tg getGame
    let at k = \case
          Object o -> KeyMap.lookup k o
          _ -> Nothing
        info f = at f =<< at "campaign" view
    info "name" `shouldBe` Just (String "Hydra Rising")
    info "scenario" `shouldBe` Just (Number 2)
    info "scenarios" `shouldBe` Just (Number 5)
    info "mode" `shouldBe` Just (String "ExpertCampaign")

  it "a permanent upgrade is not offered to an effect that would discard it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      basic <- putIntoPlay p "04161a"
      shooter <- putIntoPlay p "01008"
      _ <- revealNow p Treacheries.chaosInThePrison
      choose "discard an upgrade" (isLabel "$label.discardAnUpgradeYouControl")
      offers (isTargetChoice (upgradeIdOf basic)) `shouldReturn` False
      offers (isTargetChoice (upgradeIdOf shooter)) `shouldReturn` True

  it "an effect choosing an upgrade to discard has nothing to choose but a permanent one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      basic <- putIntoPlay p "04161a"
      _ <- revealNow p Treacheries.electricWhipAttack
      choose "discard an upgrade" (isLabel "$label.discardAnUpgradeYouControl")
      offers (isTargetChoice (upgradeIdOf basic)) `shouldReturn` False
      upgradeCodes >>= (`shouldSatisfy` elem "04161a")

  it "a Tech upgrade used is crossed out of the campaign log" do
    let c = recordInLog (RecordTechUpgrade spiderMan "04156") (campaignAt StandardCampaign 1)
    tg <- dealCampaign c [withCard "04156" (starter "spider-man")]
    inGame tg do
      toFirstTurn
      let p = player1 tg
      side <- (.playerIdentitySide) <$> playerAttrs p
      when (side /= A) (changeForm p)
      before <- length <$> handOf p
      [uid] <- upgradesOf p
      choose "use Tactical Scanner" (isAbilityOf (UpgradeRef uid))
      length <$> handOf p `shouldReturn` (before + 5)
      upgradeCodes >>= (`shouldSatisfy` notElem "04156")
      (.techUpgrade) <$> spiderManNow `shouldReturn` Nothing

  it "deals the next game's deck with the cards the log adds, the allies removed taken out" $ id @(IO ()) do
    let c =
          recordInLog (RecordAlliesRemoved ["01002"]) $
            recordInLog (RecordTechUpgrade spiderMan "04158") (campaignAt StandardCampaign 3)
        deck = campaignDeck c (starter "spider-man")
    map fst deck.cards `shouldSatisfy` elem "04158"
    map fst deck.cards `shouldSatisfy` notElem "01002"

  it "a deck the campaign took an ally from must be brought back to 40 cards, permanent ones not counted" $ id @(IO ()) do
    let deck = withCard "04159a" (starter "spider-man")
        removed = recordInLog (RecordAlliesRemoved ["01002"]) (campaignAt StandardCampaign 4)
        total = sum [n | (code, n) <- deck.cards, code /= "04159a"]
        ally = sum [n | (code, n) <- deck.cards, code == "01002"]
    campaignDeckShortBy (campaignAt StandardCampaign 4) deck `shouldBe` max 0 (40 - total)
    campaignDeckShortBy removed deck `shouldBe` max 0 (40 - (total - ally))
    (total - ally < 40) `shouldBe` True

  it "an expert campaign's obligation drawn goes into its player's play area" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      handSize <- handSizeOf p
      _ <- stackDeck p ["04165"]
      resolve [IdentityMessage p (DrawCards FromDeck 1)]
      map toCardCode <$> handOf p >>= (`shouldSatisfy` notElem "04165")
      owners <- getsGame (map (obligationOwner . toAttrs) . foldr (:) [] . gameObligations)
      owners `shouldBe` [Just p]
      handSizeOf p `shouldReturn` (handSize - 1)
