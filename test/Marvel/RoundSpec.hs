module Marvel.RoundSpec (spec) where

import Data.Aeson (Result (..), fromJSON)
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.Text qualified as T
import Marvel.Log.Entry (LogEntry (..), LogPart (..))
import Marvel.Phase (Phase (..))
import Marvel.SideScheme.Cards qualified as SideScheme
import TestImport

-- | The heading an entry opens, if it is one of the round's.
heading :: FrontendMessage -> Maybe Text
heading = \case
  Logged e | LogI18n key _ : _ <- e.logEntryBody, any (`T.isPrefixOf` key) ["log.step.", "log.phase.villain", "log.round"] -> Just key
  _ -> Nothing

spec :: Spec
spec = describe "the round as the board shows it" do
  it "animates a revealed side scheme only once" do
    let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
    told <- newIORef []
    (app, _) <- dealtGameTelling id setup [starter "spider-man"] 1 \m -> modifyIORef' told (<> [m])
    card <- runGameApp app do
      toFirstTurn
      p <- getsGame (head . gameSeats)
      liftIO $ writeIORef told []
      revealNow p SideScheme.crowdControl
    said <- readIORef told
    [(cid, code) | Revealing cid code False <- said] `shouldBe` [(ecCardId card, "01108")]

  for_ [("01053", IdentityStunned, "stunned"), ("01060", IdentityConfused, "confused")] \(code, status, name) ->
    it ("shows the event and " <> show name <> " status when the event is canceled") do
      let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
      told <- newIORef []
      (app, _) <- dealtGameTelling id setup [starter "she-hulk"] 1 \m -> modifyIORef' told (<> [m])
      runGameApp app do
        toFirstTurn
        p <- getsGame (head . gameSeats)
        clearHand p
        event <- putInHand p code
        strength <- putInHand p "01090"
        placeMainThreat 6
        changeForm p >> pass
        resolve [IdentityMessage p status]
        liftIO $ writeIORef told []
        playWith event [strength]
      said <- readIORef told
      [(c, ss) | EventCanceled c ss <- said] `shouldBe` [(code, [name])]

  it "shows the board as each step of the villain phase begins, every round alike" do
    let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
    told <- newIORef []
    (app, _) <- dealtGameTelling id setup [starter "spider-man"] 1 \m -> modifyIORef' told (<> [m])
    runGameApp app do
      toFirstTurn
      liftIO $ writeIORef told []
      endTurn
      pass
    said <- readIORef told
    -- each heading with what was shown just before it
    let headed = [(key, before) | (before, m) <- zip (Nothing : map Just said) said, Just key <- [heading m]]
        boardBefore key = case List.lookup key headed of
          Just (Just (Board b)) -> Just b
          _ -> Nothing
    map fst headed
      `shouldSatisfy` isPrefixOf
        [ "log.step.endPlayerPhase"
        , "log.phase.villain"
        , "log.step.placeThreat"
        , "log.step.enemiesActivate"
        , "log.step.dealEncounterCards"
        , "log.step.revealEncounterCards"
        ]
    for_ (map fst headed) \key -> (key, isJust (boardBefore key)) `shouldBe` (key, True)
    -- step 1 is seen on its own: in the villain phase, its threat placed before step 2
    let phaseOf b = gamePhase <$> resultToMaybe (fromJSON b :: Result Game)
        placing = boardBefore "log.step.placeThreat"
        placed = boardBefore "log.step.enemiesActivate"
    (phaseOf =<< placing) `shouldBe` Just VillainPhase
    placing `shouldSatisfy` (/= placed)
 where
  resultToMaybe :: Result a -> Maybe a
  resultToMaybe = \case
    Success a -> Just a
    Error _ -> Nothing
