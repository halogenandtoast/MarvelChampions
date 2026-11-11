module Marvel.Rules.DiscardDisplaySpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

shownDiscards :: Value -> [Object]
shownDiscards = \case
  Object obj | Just (Array cards) <- KeyMap.lookup "discardedCards" obj -> [card | Object card <- toList cards]
  _ -> []

sources :: Value -> [Value]
sources = \case
  Object obj | Just (Object questions) <- KeyMap.lookup "question" obj ->
    [source | Object q <- KeyMap.elems questions, Just source <- [KeyMap.lookup "source" q]]
  _ -> []

declineCounts :: GameAppT ()
declineCounts = whenM (offers isNoResponse) $ choose "decline the icon interrupt" isNoResponse >> declineCounts

spec :: Spec
spec = describe "Discards used by resolving effects" do
  it "shows both Molecular Decay discards and highlights each as its icons are counted" do
    tg <- testGame ["scarlet-witch"]
    inGame tg do
      let p = player1 tg
      asHero p
      (cards, _) <- setEncounterDeck [Treacheries.explosion, Treacheries.stampede, Treacheries.advance] []
      decay <- putInHand p "15005"
      payment <- traverse (putInHand p) ["01088", "01089"]
      playWith decay payment
      chooseVillain
      first <- liftIO . publicGame =<< getGame
      map (KeyMap.lookup "id") (shownDiscards first) `shouldBe` map (Just . toJSON . ecCardId) (take 2 cards)
      sources first `shouldBe` [toJSON $ ecCardId $ testHead cards]
      choose "leave the first count unchanged" isNoResponse
      second <- liftIO . publicGame =<< getGame
      sources second `shouldBe` [toJSON $ ecCardId $ cards !! 1]
      declineCounts
      final <- liftIO . publicGame =<< getGame
      shownDiscards final `shouldBe` []
      villainDamageNow `shouldReturn` 8

  it "retains Chaos Control's replacement discard beside the original group" do
    tg <- testGame ["scarlet-witch"]
    inGame tg do
      let p = player1 tg
      asHero p
      crest <- putIntoPlay p "15009"
      (cards, _) <- setEncounterDeck [Treacheries.explosion, Treacheries.stampede, Treacheries.advance, Treacheries.assault] []
      decay <- putInHand p "15005"
      payment <- traverse (putInHand p) ["01088", "01089"]
      playWith decay payment
      chooseVillain
      choose "Chaos Control" (isNamedAbility "Chaos Control")
      view <- liftIO . publicGame =<< getGame
      map (KeyMap.lookup "id") (shownDiscards view) `shouldBe` map (Just . toJSON . ecCardId) (take 3 cards)
      sources view `shouldBe` [toJSON $ ecCardId $ cards !! 2]
      game <- getGame
      queue <- getQueue >>= liftIO . readIORef
      legacy <- liftIO $ publicGame $ restoreResolvingEvents (game {gameBoostIconReplacements = Nothing}) queue
      sources legacy `shouldBe` [toJSON $ ecCardId $ cards !! 2]
      choose "Crest modifies the replacement count" (isAbilityOfCard crest)
      crestPrompt <- liftIO . publicGame =<< getGame
      sources crestPrompt `shouldBe` [toJSON $ ecCardId $ cards !! 2]
      choose "increase replacement icons" (isLabel "$label.increaseBoostIcons")
      declineCounts
      villainDamageNow `shouldReturn` 7
      gameBoostIconReplacements <$> getGame `shouldReturn` Just []
      shownDiscards <$> (liftIO . publicGame =<< getGame) `shouldReturn` []

  it "restores an old Molecular Decay prompt from the pending icon-count callback" do
    tg <- testGame ["scarlet-witch"]
    inGame tg do
      let p = player1 tg
      asHero p
      (cards, _) <- setEncounterDeck [Treacheries.explosion, Treacheries.stampede, Treacheries.advance] []
      decay <- putInHand p "15005"
      payment <- traverse (putInHand p) ["01088", "01089"]
      playWith decay payment
      chooseVillain
      game <- getGame
      queue <- getQueue >>= liftIO . readIORef
      let restored = restoreResolvingEvents (game {gameDiscardDisplays = Nothing}) queue
      view <- liftIO $ publicGame restored
      map (KeyMap.lookup "id") (shownDiscards view) `shouldBe` map (Just . toJSON . ecCardId) (take 2 cards)

  it "shows inspected player-deck discards as well as encounter-deck discards" do
    original <- testGame ["iron-man"]
    captured <- newIORef []
    let tg = original {app = original.app {appFrontend = \message -> do
          original.app.appFrontend message
          case message of
            Board value | Success game <- fromJSON value -> modifyIORef' captured (<> [game :: Game])
            _ -> pure ()}}
    inGame tg do
      let p = player1 tg
      asHero p
      cards <- stackDeck p ["01088", "01089", "01090", "01087", "01086", "01005"]
      blast <- putInHand p "01031"
      resource <- putInHand p "01089"
      playWith blast [resource]
      chooseVillain
      snapshots <- liftIO $ readIORef captured
      let displayed = [map toCardId (concatMap discardDisplayCards frames) | game <- snapshots, Just frames <- [gameDiscardDisplays game], not (null frames)]
      displayed `shouldSatisfy` elem (map pcCardId $ take 5 cards)
      shownDiscards <$> (liftIO . publicGame =<< getGame) `shouldReturn` []
