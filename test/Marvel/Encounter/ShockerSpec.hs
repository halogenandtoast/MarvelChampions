module Marvel.Encounter.ShockerSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import TestImport

revealWithoutAcknowledging :: IdentityId -> GameAppT ()
revealWithoutAcknowledging you = do
  card <- encounterCard Minions.shocker
  withGame_ $ questionL .~ mempty
  pushAll [RevealEncounterCard you card]
  runGameMessages

spec :: Spec
spec = describe "Shocker's reveal" do
  it "does not pause for an effect with no heroes in play" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let you = player1 tg
      revealWithoutAcknowledging you
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.resolveWhenRevealed" _ _) -> liftIO $ expectationFailure "no heroes can be damaged"
        _ -> pure ()
      length . HashMap.elems <$> getsGame gameMinions `shouldReturn` 1
      damageOf you `shouldReturn` 0
      damageOf (player2 tg) `shouldReturn` 0

  it "pauses when another player is a hero and damages only heroes" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let you = player1 tg
          hero = player2 tg
      resolve [IdentityMessage hero (ChangedToForm A)]
      pass
      revealWithoutAcknowledging you
      openQuestion >>= \case
        Just (asked, QuestionLabel "$label.resolveWhenRevealed" _ _) -> answerQuestion asked 0
        _ -> liftIO $ expectationFailure "expected Shocker's reveal acknowledgment"
      pass
      damageOf you `shouldReturn` 0
      damageOf hero `shouldReturn` 1
