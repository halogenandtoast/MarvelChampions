module Marvel.PublicSpec (spec) where

import Control.Monad (replicateM_)
import Data.ByteString.Lazy qualified as BL
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Deck
import Marvel.Game.Public (ChoiceView (..), describeChoice, owedFor, publicGame)
import System.Environment (lookupEnv)
import TestImport

spec :: Spec
spec = describe "the game as the client sees it" do
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

  it "says what a cost still needs once resources are paid" $ id @(IO ()) do
    owedFor [Just Physical, Nothing, Nothing] [] `shouldBe` [Just Physical, Nothing, Nothing]
    owedFor [Just Physical, Nothing, Nothing] [Energy] `shouldBe` [Just Physical, Nothing]
    owedFor [Just Physical, Nothing] [Wild, Mental] `shouldBe` []
    owedFor [Nothing] [Physical, Mental] `shouldBe` []

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
