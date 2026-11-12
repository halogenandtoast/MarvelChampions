module Marvel.Encounter.KlawSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.EncounterSet (EncounterSet (MastersOfEvil))
import TestImport

revealPrompt :: MainSchemeId -> GameAppT IdentityId
revealPrompt sid = openQuestion >>= \case
  Just (you, QuestionLabel "$label.resolveWhenRevealed" _ (ChooseOne [TargetLabel target _])) -> do
    target `shouldBe` MainSchemeRef sid
    pure you
  other -> error $ "expected the main scheme's When Revealed prompt, got " <> show other

spec :: Spec
spec = describe "Klaw main schemes" do
  it "prompts for Underground Distribution's When Revealed after Setup, before discarding and engaging a minion" do
    (app, _) <- dealtGameBeforeSetup id (ScenarioSetup "01113" [MastersOfEvil] Normal) [starter "groot"] 1 (const $ pure ())
    runGameApp app do
      sid <- mainSchemeIdNow
      openQuestion >>= \case
        Just (you, QuestionLabel "$label.setupMainScheme" _ _) -> answerQuestion you 0
        other -> error $ "expected Setup, got " <> show other
      -- Side A reveals Defense Network first; its own When Revealed is separate
      -- from Underground Distribution's on side B.
      openQuestion >>= \case
        Just (you, QuestionLabel "$label.resolveWhenRevealed" _ (ChooseOne [TargetLabel (SideSchemeRef _) _])) -> answerQuestion you 0
        other -> error $ "expected Defense Network's When Revealed, got " <> show other
      you <- revealPrompt sid
      getsGame (HashMap.size . gameMinions) `shouldReturn` 0
      before <- encounterDeckSize
      answerQuestion you 0
      minions <- getsGame (map toAttrs . HashMap.elems . gameMinions)
      length minions `shouldBe` 1
      map minionEngagedIdentity minions `shouldBe` [you]
      after <- encounterDeckSize
      (after < before) `shouldBe` True

  it "prompts for Secret Rendezvous's When Revealed when the main scheme advances" do
    tg <- testGameAgainst "01113" ["spider-man"]
    inGame tg do
      before <- getsGame (HashMap.size . gameMinions)
      old <- mainSchemeIdNow
      withGame_ $ questionL .~ mempty
      push $ AdvanceMainScheme old
      runGame
      sid <- mainSchemeIdNow
      you <- revealPrompt sid
      getsGame (HashMap.size . gameMinions) `shouldReturn` before
      answerQuestion you 0
      getsGame (HashMap.size . gameMinions) `shouldReturn` (before + 1)
