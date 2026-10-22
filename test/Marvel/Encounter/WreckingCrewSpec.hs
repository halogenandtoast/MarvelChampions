-- | The Wrecking Crew (Breakout): four villains, each with his own side scheme and
-- encounter deck, one of them active (TWC insert pp.6-7).
module Marvel.Encounter.WreckingCrewSpec (spec) where

import Control.Monad ((<=<))
import Data.List (sort)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Area
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Matchers
import Marvel.Message.Lifted (signatureSchemeOf)
import Marvel.Query
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

breakout :: [Text] -> IO TestGame
breakout = testGameAgainst "07001"

-- | The villain in play by his card's code.
villainCoded :: CardCode -> GameAppT VillainId
villainCoded code =
  getsGame (HashMap.toList . gameVillains) >>= \vs -> case [v | (v, x) <- vs, toCardCode (getCardDef x) == code] of
    [v] -> pure v
    _ -> error $ "no villain " <> show code

wrecker, thunderball, piledriver, bulldozer :: GameAppT VillainId
wrecker = villainCoded "07002"
thunderball = villainCoded "07017"
piledriver = villainCoded "07032"
bulldozer = villainCoded "07046"

ownScheme :: VillainId -> GameAppT SideSchemeId
ownScheme v = fromMaybe (error "no side scheme") <$> signatureSchemeOf v

-- | Exactly so much threat on the side scheme (under 10, so nothing fires).
setThreat :: SideSchemeId -> Natural -> GameAppT ()
setThreat sid n = resolve $ SideSchemeMessage sid (SideSchemeThreatRemoved GameRef 99) : [SideSchemeMessage sid (SideSchemePlaceThreat n) | n > 0]

-- | The choice made now, as the player's only answer.
answerNow :: IdentityId -> Choice -> GameAppT ()
answerNow p c = do
  runNow [Ask p (ChooseOne [c])]
  answerQuestion p 0

activeNow :: GameAppT VillainId
activeNow = selectJust ActiveVillain

deckSize :: VillainId -> GameAppT Int
deckSize v = length . pileDeck <$> getPile (VillainDeckKey v)

spec :: Spec
spec = describe "The Wrecking Crew" do
  it "sets up four villains, each with his own side scheme and 15-card deck, Wrecker active" do
    tg <- breakout ["spider-man"]
    inGame tg do
      vs <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      activeNow `shouldReturn` fromMaybe (error "no villains") (headMay vs)
      threats <- traverse (sideSchemeThreat <=< ownScheme) vs
      threats `shouldBe` [6, 5, 3, 4]
      traverse deckSize vs `shouldReturn` [15, 15, 15, 15]
      encounterDeckSize `shouldReturn` 0

  it "deals encounter cards and boost cards from the active villain's deck" do
    tg <- breakout ["spider-man"]
    let p = player1 tg
    inGame tg do
      w <- wrecker
      t <- thunderball
      resolve [SetActiveVillain t]
      runNow [DealEncounterCard p]
      deckSize t `shouldReturn` 14
      deckSize w `shouldReturn` 15
      runNow [DealBoost (VillainRef w)]
      deckSize t `shouldReturn` 13
      deckSize w `shouldReturn` 15

  it "a villain schemes onto his own side scheme, not the main scheme" do
    tg <- breakout ["spider-man"]
    inGame tg do
      pd <- piledriver
      own <- ownScheme pd
      before <- mainSchemeThreatNow
      runNow [VillainMessage pd VillainSchemes]
      mainSchemeThreatNow `shouldReturn` before
      sideSchemeThreat own >>= (`shouldSatisfy` (>= 5))

  it "is won only once all four villains are defeated" do
    tg <- breakout ["spider-man"]
    inGame tg do
      vs <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      for_ (take 3 vs) \v -> resolve [ResolveVillainDefeat v]
      getsGame gameState `shouldReturn` InProgress
      getsGame (HashMap.size . gameSideSchemes) `shouldReturn` 1
      resolve [ResolveVillainDefeat (vs !! 3)]
      getsGame gameState `shouldReturn` Finished Won

  it "passes the active counter, the active villain defeated, to the villain whose side scheme has the most threat, the first player breaking a tie" do
    tg <- breakout ["spider-man"]
    inGame tg do
      [w, t, pd, b] <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      for_ [(t, 7), (pd, 7), (b, 2)] \(v, n) -> (`setThreat` n) =<< ownScheme v
      runNow [ResolveVillainDefeat w]
      asked >>= \case
        Just (_, cs) -> do
          length cs `shouldBe` 2
          cs `shouldSatisfy` any (isTargetChoice pd)
          cs `shouldSatisfy` any (isTargetChoice t)
        Nothing -> error "no tie-break asked"
      choose "choose Piledriver" (isTargetChoice pd)
      activeNow `shouldReturn` pd

  it "moves the active counter at once when one villain has the most threat" do
    tg <- breakout ["spider-man"]
    inGame tg do
      [w, _, _, b] <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      (`setThreat` 9) =<< ownScheme b
      resolve [ResolveVillainDefeat w]
      activeNow `shouldReturn` b

  it "Tactical Prowess moves all threat from the least to the most; gaining surge if the scheme's forced response does not fire" do
    tg <- breakout ["spider-man"]
    let p = player1 tg
    inGame tg do
      [w, t, pd, b] <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      [sw, st, sp, sb] <- traverse ownScheme [w, t, pd, b]
      for_ [(sw, 6), (st, 5), (sp, 1), (sb, 4)] (uncurry setThreat)
      resolve [SetActiveVillain t]
      dealt <- length <$> dealtTo p
      _ <- revealNow p Treacheries.tacticalProwess
      sideSchemeThreat sp `shouldReturn` 0
      sideSchemeThreat sw `shouldReturn` 7
      -- surge: another card dealt
      length <$> dealtTo p `shouldReturn` dealt + 1

  it "Tactical Prowess firing a scheme's forced response does not surge" do
    tg <- breakout ["spider-man"]
    let p = player1 tg
    inGame tg do
      [w, t, pd, b] <- traverse id [wrecker, thunderball, piledriver, bulldozer]
      [sw, st, sp, sb] <- traverse ownScheme [w, t, pd, b]
      for_ [(sw, 8), (st, 5), (sp, 3), (sb, 4)] (uncurry setThreat)
      dealt <- length <$> dealtTo p
      _ <- revealNow p Treacheries.tacticalProwess
      sideSchemeThreat sp `shouldReturn` 0
      -- Day of Reckoning reached 11: its Hard Hitter dealt 2 damage, then all but 3 went
      sideSchemeThreat sw `shouldReturn` 3
      damageOf p `shouldReturn` 2
      length <$> dealtTo p `shouldReturn` dealt

  it "Held Hostage attaches to the active villain's side scheme, which thwarting cannot remove threat from; its hero action has that villain attack" do
    tg <- breakout ["spider-man"]
    let p = player1 tg
    inGame tg do
      t <- thunderball
      st <- ownScheme t
      resolve [SetActiveVillain t]
      card <- revealNow p Attachments.heldHostageThunderball
      let hostage = AttachmentId $ unCardId $ ecCardId card
      setThreat st 6
      hero <- selectAny (IdentityWithId p <> HeroIdentity)
      unless hero $ changeForm p
      -- a basic thwart removes none
      answerNow p (BasicThwartScheme (SideSchemeRef st) (IdentityRef p) 2)
      sideSchemeThreat st `shouldReturn` 6
      -- nor does an ability labeled a thwart: For Justice!, "Hero Action (thwart)"
      clearHand p
      forJustice <- putInHand p "01060"
      payment <- traverse (putInHand p) ["01088", "01088"]
      playWith forJustice payment
      choose "thwart Thunderstruck" (isTargetChoice (SchemeSideSchemeId st))
      sideSchemeThreat st `shouldReturn` 6
      -- a "remove threat" that is not a thwart still removes it
      answerNow p (ThwartScheme (SideSchemeRef st) GameRef 2)
      sideSchemeThreat st `shouldReturn` 4
      choose "use Held Hostage" (isAbilityOf (AttachmentRef hostage))
      ranMessage tg (VillainMessage t (VillainAttacks p)) `shouldReturn` True
      -- Spider-Man's sense for the attack is not used
      whenM (offers isNoResponse) $ choose "not respond" isNoResponse
      choose "not defend" isNoDefender
      -- Thunderball's forced response to attacking, and on to the turn again
      toFirstTurn
      getsGame (HashMap.member hostage . gameAttachments) `shouldReturn` False

  it "in the extreme variant a defeated version A gives way to version B, which must be defeated too" do
    tg <- testGameAgainst "07001a" ["spider-man"]
    inGame tg do
      w <- wrecker
      own <- ownScheme w
      resolve [VillainMessage w VillainDefeated]
      getsGame (fmap (toCardCode . getCardDef) . HashMap.lookup w . gameVillains) `shouldReturn` Just "07003"
      getsGame (HashMap.member own . gameSideSchemes) `shouldReturn` True
      resolve [VillainMessage w VillainDefeated]
      getsGame (HashMap.member w . gameVillains) `shouldReturn` False

  it "expert mode plays the version B villains" do
    tg <- expertBreakout ["spider-man"]
    inGame tg do
      codes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameVillains)
      sort codes `shouldBe` ["07003", "07018", "07033", "07047"]
 where
  ranMessage tg msg = elem (tshow msg) <$> liftIO (readIORef tg.ran)

expertBreakout :: [Text] -> IO TestGame
expertBreakout heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "07001" [] Expert) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg
