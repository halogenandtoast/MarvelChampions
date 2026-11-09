module Marvel.PublicSpec (spec) where

import Control.Monad (replicateM_)
import Data.ByteString.Lazy qualified as BL
import Data.Aeson.Key (Key)
import Data.Text qualified as T
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Scenario.Types qualified as Scenario
import Marvel.Hercules.Cards qualified as Hercules
import Marvel.Ability (ability, AbilityType (HeroAction))
import Marvel.Criteria (Criteria (OwnsThis))
import Marvel.Game.Refresh (refreshTurnQuestions)
import Marvel.ActiveCost.Types (ActiveCost (..), ActiveCostTarget (ForTreachery))
import Marvel.Cost.Types (Cost (..))
import Marvel.Message.Deck
import Marvel.Game.Public (ChoiceView (..), describeChoice, owedFor, publicGame, publicGameFor)
import System.Environment (lookupEnv)
import TestImport

spec :: Spec
spec = describe "the game as the client sees it" do
  it "applies direct debug tokens and statuses without losing the waiting question" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      waiting <- getsGame gameQuestion
      runDebug $ CardSpecific (IdentityRef p) "debugSetValues" $ object
        ["damage" .= (3 :: Natural), "counters" .= (4 :: Natural), "exhausted" .= True,
         "stunned" .= True, "additionalStun" .= True, "confused" .= True, "tough" .= True, "additionalTough" .= (2 :: Natural)]
      attrs <- playerAttrs p
      playerIdentityDamage attrs `shouldBe` 3
      playerIdentityCounters attrs `shouldBe` 4
      playerIdentityExhausted attrs `shouldBe` True
      playerIdentityStunned attrs `shouldBe` True
      playerIdentityAdditionalStun attrs `shouldBe` True
      playerIdentityConfused attrs `shouldBe` True
      playerIdentityAdditionalTough attrs `shouldBe` 2
      getsGame gameQuestion `shouldReturn` waiting
      runDebug $ CardSpecific (IdentityRef p) "debugSetValues" $ object ["confused" .= False]
      playerIdentityConfused <$> playerAttrs p `shouldReturn` False
    told <- T.concat . map entryText <$> readIORef tg.logged
    told `shouldSatisfy` (not . T.isInfixOf "$log.thwartCanceledByConfusion")

  it "rejects invalid debug edits atomically and preserves card-specific metadata" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      before <- getPlayer p
      for_ [object ["damage" .= (-1 :: Int)], object ["damage" .= (1.5 :: Double)], object ["counters" .= (4 :: Int), "hp" .= (1 :: Int)], object ["damage" .= True]] \values -> do
        runDebug $ CardSpecific (IdentityRef p) "debugSetValues" values
        getPlayer p `shouldReturn` before
      guild <- putIntoPlay p "37003"
      let sid = SupportId $ unCardId $ toCardId guild
      original <- getsGame $ lookup sid . gameSupports
      runDebug $ CardSpecific (SupportRef sid) "debugSetValues" $ object ["uses" .= (5 :: Natural), "exhausted" .= True]
      edited <- getsGame $ lookup sid . gameSupports
      let withoutEdit entity = case toJSON entity of
            Object fields -> Object $ KeyMap.delete "supportUses" $ KeyMap.delete "supportExhausted" fields
            value -> value
      withoutEdit edited `shouldBe` withoutEdit original
      edited `shouldSatisfy` (/= original)
      wolfsbane <- putIntoPlay p "40051"
      let aid = AllyId $ unCardId $ toCardId wolfsbane
      originalAlly <- getsGame $ lookup aid . gameAllies
      runDebug $ CardSpecific (AllyRef aid) "debugSetValues" $ object ["damage" .= (1 :: Natural), "counters" .= (2 :: Natural), "stunned" .= True, "additionalStun" .= True]
      editedAlly <- getsGame $ lookup aid . gameAllies
      let unchangedFields entity = case toJSON entity of
            Object fields -> Object $ foldr KeyMap.delete fields ["allyDamage", "allyCounters", "allyStunned", "allyAdditionalStun"]
            value -> value
      unchangedFields editedAlly `shouldBe` unchangedFields originalAlly
      editedAlly `shouldSatisfy` (/= originalAlly)
      main <- mainSchemeIdNow
      runDebug $ CardSpecific (MainSchemeRef main) "debugSetValues" $ object ["threat" .= (99 :: Natural), "counters" .= (2 :: Natural), "accelerationTokens" .= (3 :: Natural)]
      mainSchemeThreatNow `shouldReturn` 99

  it "shows set-aside cards to every viewer and removes cards when they leave that area" do
    tg <- testGame ["rogue"]
    inGame tg do
      cards <- getsGame $ Scenario.scenarioSetAsideCards . toAttrs . gameScenario
      cards `shouldSatisfy` notNull
      let shown view = [(cid, code) | Object o <- [view], Just (Array entries) <- [KeyMap.lookup "setAsideCards" o], Object card <- toList entries, Just cid <- [KeyMap.lookup "id" card], Just code <- [KeyMap.lookup "code" card]]
          expected = [(toJSON $ toCardId card, toJSON $ toCardCode card) | card <- cards]
      view <- getGame >>= liftIO . publicGameFor (const False)
      shown view `shouldBe` expected
      for_ (headMay cards) $ \card -> do
        runNow [RemoveFromGame $ CardIdRef $ toCardId card]
        updated <- getGame >>= liftIO . publicGameFor (const False)
        shown updated `shouldBe` filter ((/= toJSON (toCardId card)) . fst) expected

  it "loads stored games from before phase history was tracked" do
    tg <- testGame ["spider-man"]
    game <- inGame tg getGame
    let legacy = case toJSON game of
          Object o -> Object (KeyMap.delete "playedThisPhase" o)
          value -> value
    fromJSON legacy `shouldBe` Success (game {gamePlayedThisPhase = []})

  it "preserves phase history when loading a current stored game" do
    tg <- testGame ["spider-man"]
    game <- inGame tg getGame
    let saved = game {gamePlayedThisPhase = [(player1 tg, lookupPlayerCard "01083")]}
    fromJSON (toJSON saved) `shouldBe` Success saved

  it "shows the villain's boosted ATK for its current attack, then its normal ATK" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      vid <- villainIdNow
      normal <- shownAttacks "villains"
      normal `shouldSatisfy` (not . null)
      runNow [VillainMessage vid (VillainAttacks p)]
      runNow [VillainMessage vid (VillainAddBoost 3)]
      shownAttacks "villains" `shouldReturn` map (+ 3) normal
      runNow [VillainMessage vid VillainEndAttack]
      shownAttacks "villains" `shouldReturn` normal

  it "shows a minion's boosted ATK for its current attack, then its normal ATK" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      card <- revealNow p Minions.hydraMercenary
      pass
      let mid = minionOf card
      normal <- shownAttacks "minions"
      normal `shouldSatisfy` (not . null)
      runNow [MinionMessage mid (MinionAttacks p)]
      runNow [MinionMessage mid (MinionAddBoost 2)]
      shownAttacks "minions" `shouldReturn` map (+ 2) normal
      runNow [MinionMessage mid MinionEndAttack]
      shownAttacks "minions" `shouldReturn` normal

  it "describes a dealt table, and is written out when SAMPLE_VIEW names a file" do
    let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
    (app, _) <- dealtGame setup [starter "spider-man", starter "captain-marvel"] 3
    -- the player order, then a little way into the first turn
    runGameApp app $ replicateM_ 3 (openQuestion >>= traverse_ (\(ident, _) -> answerQuestion ident 0))
    view <- publicGame =<< runGameApp app getGame
    lookupEnv "SAMPLE_VIEW" >>= traverse_ \path -> BL.writeFile path (encode view)
    case view of
      Object _ -> pure ()
      _ -> expectationFailure "the view is not an object"

  it "refreshes a saved turn without reviving Team-Building Exercise's old action" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      team <- putIntoPlay p "12024"
      let stale = ability (supportIdOf team) 1 HeroAction OwnsThis ExhaustCost (Run [])
      withGame_ $ questionL .~ fromList [(p, QuestionLabel "$label.yourTurn" Nothing $ ChooseOne [EndTurn, UseAbility stale])]
      saved <- getGame
      refreshed <- liftIO $ refreshTurnQuestions saved
      liftIO $ gameQuestion refreshed `shouldNotBe` gameQuestion saved
      refreshed {gameQuestion = gameQuestion saved} `shouldBe` saved
      getGame `shouldReturn` saved
      -- The view and the engine answering its indices must see the same new menu.
      view <- liftIO $ publicGame saved
      let offered = [target | Object o <- [view], Just (Object questions) <- [KeyMap.lookup "question" o], Object question <- KeyMap.elems questions,
            Just (Array choices) <- [KeyMap.lookup "choices" question], Object choice <- toList choices, Just target <- [KeyMap.lookup "target" choice]]
      liftIO $ offered `shouldNotContain` [toJSON $ tshow $ supportIdOf team]
      let responding = saved {gameQuestion = fromList [(p, QuestionLabel "$label.respond" Nothing $ ChooseOne [UseAbility stale])]}
      liftIO (refreshTurnQuestions responding) `shouldReturn` responding

  it "says what a cost still needs once resources are paid" $ id @(IO ()) do
    owedFor [Just Physical, Nothing, Nothing] [] `shouldBe` [Just Physical, Nothing, Nothing]
    owedFor [Just Physical, Nothing, Nothing] [Energy] `shouldBe` [Just Physical, Nothing]
    owedFor [Just Physical, Nothing] [Wild, Mental] `shouldBe` []
    owedFor [Nothing] [Physical, Mental] `shouldBe` []

  it "reports excess resources using the current cost and resource types, excluding X costs" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          twoEnergy = ResourcePayment Energy <> ResourcePayment Energy
          cases :: [(Cost, Payment, [Int])]
          cases =
            [ (ResourceCost Nothing, twoEnergy, [1])
            , (ResourceCost (Just Physical), twoEnergy, [0, 1])
            , (MultiResourceCost [Nothing, Nothing], twoEnergy, [])
            , (ResourceCost Nothing, twoEnergy <> ActualResourcesPayment [Energy], [1])
            , (ResourceCost (Just Physical), ResourcePayment Energy <> ResourcePayment Physical, [0])
            , (ResourceCost (Just Physical), WildPayment Wild <> ResourcePayment Physical, [0])
            , (DynamicResourceCost Nothing, twoEnergy, [])
            , (PrintedResourceCost [Physical], ResourcePayment Wild <> PrintedResourcesPayment [Wild], [0])
            , (Costs [ResourceCost Nothing, MayOverpay], ResourcePayment Energy <> OverpaymentOnlyPayment Energy, [1])
            ]
      for_ cases $ \(cost, payment, expected) -> do
        ident <- getRandom
        withGame_ $ activeCostL .~ [ActiveCost ident p ForTreachery cost payment Nothing [] Nothing]
        view <- liftIO . publicGame =<< getGame
        let surplus = case view of
              Object o | Just (Object active) <- KeyMap.lookup "activeCost" o -> KeyMap.lookup "overpaidResources" active
              _ -> Nothing
        surplus `shouldBe` Just (toJSON expected)

  it "reports matching and distinct resource requirements using the payment rules" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          cases :: [(Cost, [Resource], Text, Bool, [Int])]
          cases =
            [ (SameResourcesCost 2, [Physical, Mental], "same", False, [])
            , (SameResourcesCost 2, [Physical, Physical], "same", True, [])
            , (SameResourcesCost 2, [Physical, Wild], "same", True, [])
            , (SameResourcesCost 2, [Physical, Mental, Energy, Energy], "same", True, [0, 1])
            , (DifferentResourcesCost 2, [Physical, Physical], "different", False, [])
            , (DifferentResourcesCost 2, [Physical, Mental], "different", True, [])
            , (DifferentResourcesCost 2, [Physical, Wild], "different", True, [])
            , (DifferentResourcesCost 2, [Physical, Physical, Mental], "different", True, [1])
            ]
      for_ cases $ \(cost, resources, kind, met, surplus) -> do
        ident <- getRandom
        withGame_ $ activeCostL .~ [ActiveCost ident p ForTreachery cost (mconcat $ map ResourcePayment resources) Nothing [] Nothing]
        view <- liftIO . publicGame =<< getGame
        case view of
          Object o | Just (Object active) <- KeyMap.lookup "activeCost" o -> do
            KeyMap.lookup "resourceRequirements" active `shouldBe` Just (toJSON [object ["kind" .= kind, "count" .= (2 :: Int), "met" .= met]])
            KeyMap.lookup "overpaidResources" active `shouldBe` Just (toJSON surplus)
          _ -> fail "expected the active payment view"

  it "reports the remaining card picks through searched and prompted questions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          cards = [CardLabel "01088" (Run []), CardLabel "01089" (Run []), CardLabel "01090" (Run [])]
          cases :: [(Question, Int)]
          cases = [(ChooseOne cards, 1), (ChooseN 2 cards, 2), (ChooseN 2 (take 1 cards), 1), (ChooseOneAtATime cards, 3),
            (QuestionLabel "$label.searchDeck" Nothing $ Searched [] $ ChooseN 1 cards, 1)]
      for_ cases $ \(question, expected) -> do
        withGame_ $ questionL .~ fromList [(p, question)]
        view <- liftIO . publicGame =<< getGame
        let remaining = [n | Object o <- [view], Just (Object questions) <- [KeyMap.lookup "question" o], Object q <- KeyMap.elems questions, Just n <- [KeyMap.lookup "remainingPicks" q]]
        remaining `shouldBe` [toJSON expected]

  it "names a choice by what it does to what it is offered on" do
    tg <- testGame ["spider-man", "she-hulk"]
    inGame tg do
      [p1, p2] <- players
      let damage = damageIdentity p1 GameRef (toDamage 1 FromTreachery)
      (describeChoice p1 (TargetLabel (IdentityRef p1) [Run damage])).kind `shouldBe` "damage"
      -- the player chosen is not what is damaged
      (describeChoice p1 (TargetLabel (IdentityRef p2) [Run damage])).kind `shouldBe` "target"

  it "names a card choice that adds it to hand before continuing an effect" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      card <- putInHand p "01083"
      other <- putInHand p "01084"
      let body = [Run [IdentityMessage p (AddToHand card), IdentityMessage p ShuffleDeck]]
          choice = describeChoice p (TargetLabel (CardIdRef $ toCardId card) body)
      choice.kind `shouldBe` "addToHand"
      choice.target `shouldBe` Just (tshow $ toCardId card)
      (describeChoice p (TargetLabel (CardIdRef $ toCardId other) body)).kind `shouldBe` "target"

  it "names a tucked card choice by its discard action" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      card <- putInHand p "60048"
      let choice = describeChoice p (TargetLabel (CardIdRef $ toCardId card) [Run [CardSpecific (IdentityRef p) "fneReleaseTuckedCard" (toJSON card), DiscardedCard (PlayerCard card)]])
      choice.kind `shouldBe` "discard"
      choice.target `shouldBe` Just (tshow $ toCardId card)

  it "shows the card in a saved reveal choice, including a player-count override" do
    tg <- testGame ["hercules"]
    inGame tg do
      let p = player1 tg
      card <- encounterCard Schemes.breakinAndTakin
      let choice = describeChoice p $ TargetLabel (CardIdRef $ toCardId card)
            [Run [CardSpecific GameRef "revealEncounterCardWithPlayerCount" (toJSON (p, 1 :: Int, card))]]
      choice.kind `shouldBe` "card"
      choice.code `shouldBe` Just "01107"
      choice.target `shouldBe` Just (tshow $ toCardId card)

  it "corrects a saved Stealth threat-placement label without relabeling a suit change" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      uid <- getRandom
      let place = describeChoice p $ Label "$label.changeSuit"
            [Run [CardSpecific (UpgradeRef uid) "suitThreat" (toJSON (True, 2 :: Natural))]]
          change = describeChoice p $ Label "$label.changeSuit"
            [Run [CardSpecific (UpgradeRef uid) "changeSuit" (toJSON True)]]
      place.label `shouldBe` Just (countLabel "aosMoveThreat" 2)
      change.label `shouldBe` Just "$label.changeSuit"

  it "shows the flipped boost during its interrupt without exposing other boosts" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      flipped <- encounterCard Treacheries.hardToKeepDown
      hidden <- encounterCard Treacheries.advance
      withGame_ $ entitiesL . villainsL . ix vid %~ \(Villain.Villain v) ->
        Villain.Villain $ Villain.overVillainAttrs (\attrs -> attrs {Villain.villainBoostCards = [flipped, hidden]}) v
      withGame_ $ windowsL .~ [[W.Window W.When $ W.FlippedBoostCard flipped (EnemyVillainId vid)]]
      snapshot <- getGame
      view <- liftIO $ publicGameFor (const False) snapshot
      let boosts = [cards | Object o <- [view], Just (Array cards) <- [KeyMap.lookup "boostCards" o]]
          codes = [code | cards <- boosts, Object card <- toList cards, Just code <- [KeyMap.lookup "code" card]]
      codes `shouldMatchList` [String "01104", Null]
      -- Registration during the same window must not show a second copy.
      withGame_ $ boostCardsL .~ fromList [(TreacheryRef $ TreacheryId $ unCardId $ ecCardId flipped, flipped)]
      registered <- liftIO . publicGame =<< getGame
      let ids = [cid | Object o <- [registered], Just (Array cards) <- [KeyMap.lookup "boostCards" o], Object card <- toList cards, Just cid <- [KeyMap.lookup "id" card]]
      ids `shouldMatchList` [toJSON (ecCardId flipped), toJSON (ecCardId hidden)]

  it "keeps the flipped boost visible through a nested response window" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      flipped <- encounterCard Treacheries.hardToKeepDown
      withGame_ $ windowsL .~ [[W.Window W.After $ W.PlayerDeckReset p], [W.Window W.When $ W.FlippedBoostCard flipped (EnemyVillainId vid)]]
      view <- liftIO . publicGame =<< getGame
      let codes = [code | Object o <- [view], Just (Array cards) <- [KeyMap.lookup "boostCards" o], Object card <- toList cards, Just (String code) <- [KeyMap.lookup "code" card]]
      codes `shouldSatisfy` elem "01104"

  it "shows a saved inspection's card only to the inspecting player" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      card <- encounterCard Minions.hydraSoldier
      withGame_ $ questionL .~ fromList [(p, ChooseOne
        [Label "$label.done" [Run []], Label "$label.discard" [Run [DiscardedCard $ EncounterCard card]]])]
      snapshot <- getGame
      privateView <- liftIO $ publicGameFor (== p) snapshot
      hiddenView <- liftIO $ publicGameFor (const False) snapshot
      let shown view = [code | Object o <- [view], Just (Array cards) <- [KeyMap.lookup "focusedCards" o], Object c <- toList cards, Just (String code) <- [KeyMap.lookup "code" c]]
      shown privateView `shouldSatisfy` elem "01182"
      shown hiddenView `shouldSatisfy` notElem "01182"

  it "shows a revealed attachment before its When Revealed gives it a host" do
    tg <- testGame ["hercules"]
    inGame tg do
      let p = player1 tg
      card <- encounterCard Hercules.hc59003
      withGame_ $ questionL .~ mempty
      pushAll [RevealEncounterCard p card]
      runGame
      getsGame gameFocusedCards `shouldReturn` []
      view <- liftIO . publicGame =<< getGame
      let shown = [code | Object o <- [view], Just (Array cards) <- [KeyMap.lookup "focusedCards" o], Object c <- toList cards, Just (String code) <- [KeyMap.lookup "code" c]]
      shown `shouldSatisfy` elem "59003"
      let targetsSource = isTargetChoice $ AttachmentId $ unCardId $ toCardId card
      (maybe False (any targetsSource . snd) <$> asked) `shouldReturn` True

  it "keeps a played event visible through each choice until its resolution finishes" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      _ <- putIntoPlay p "62012"
      card <- putInHand p "62023"
      playWith card []
      shownResolvingEvents `shouldReturn` ["62023"]
      chooseVillain
      shownResolvingEvents `shouldReturn` ["62023"]
      choose "tough for Luke" (isTargetChoice $ IdentityCharacter p)
      pass
      shownResolvingEvents `shouldReturn` []

  it "does not show a resolved event when its card leaves the discard pile" do
    tg <- testGame ["echo"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      resolve [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "60048"
      resource <- putInHand p "01090"
      playWith card [resource]
      pass
      shownResolvingEvents `shouldReturn` []
      resolve [RemoveFromGame (CardIdRef $ toCardId card)]
      getsGame (not . null . gameEvents) `shouldReturn` True
      shownResolvingEvents `shouldReturn` []

  it "restores active events from legacy completion messages without reviving completed events" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      _ <- putIntoPlay p "62012"
      card <- putInHand p "62023"
      playWith card []
      game <- getGame
      queue <- liftIO . readIORef =<< getQueue
      let legacy = case toJSON game of
            Object o -> Object $ KeyMap.delete "resolvingEvents" o
            value -> value
      case fromJSON legacy of
        Error err -> liftIO $ expectationFailure err
        Success old -> do
          gameResolvingEvents old `shouldBe` Nothing
          gameResolvingEvents (restoreResolvingEvents old queue) `shouldBe` gameResolvingEvents game
          gameResolvingEvents (restoreResolvingEvents old []) `shouldBe` Just []
      fromJSON (toJSON game) `shouldBe` Success game

  it "shows both full searched piles when a deck-and-discard search finds no matching cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityDeck = Deck [], playerIdentityDiscard = Discard []}
      deck <- stackDeck p ["01088"]
      discard <- putInDiscard p "01089"
      runNow [IdentityMessage p $ Search (SearchIdentityDeckAndDiscardPile p) (CardWithType AllyType) SearchDrawOne ShuffleBackIn]
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.searchDeckAndDiscard" _ (Searched places _)) ->
          [(place, map toCardId cards) | (place, cards) <- places] `shouldBe`
            [(Just "$game.piles.deck", map toCardId deck), (Just "$game.piles.discard", [toCardId discard])]
        _ -> liftIO $ expectationFailure "expected both complete searched piles"
      offers (isLabel "$label.noMatchingCardsFound") `shouldReturn` True

shownResolvingEvents :: GameAppT [Text]
shownResolvingEvents = do
  view <- liftIO . publicGame =<< getGame
  pure [code | Object o <- [view], Just (Array cards) <- [KeyMap.lookup "resolvingEvents" o], Object card <- toList cards, Just (String code) <- [KeyMap.lookup "code" card]]

shownAttacks :: Key -> GameAppT [Natural]
shownAttacks field = do
  view <- liftIO . publicGame =<< getGame
  pure [floor atk | Object o <- [view], Just (Array enemies) <- [KeyMap.lookup field o], Object enemy <- toList enemies, Just (Number atk) <- [KeyMap.lookup "attack" enemy]]
