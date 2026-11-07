module Marvel.Rules.BoostRevealClickSpec (spec) where

import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Message.Lifted qualified as L
import TestImport

spec :: Spec
spec = describe "Revealing boost cards" do
  it "keeps boosts facedown until the player clicks to reveal them" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      cards <- stackEncounterDeck [Treacheries.advance]
      resolve [DealBoost $ VillainRef vid]
      let held = Villain.villainBoostCards . toAttrs <$> getVillain
      held `shouldReturn` [testHead cards]
      withGame_ $ questionL .~ mempty
      pushAll [VillainMessage vid VillainFlipBoostCards]
      runGameMessages
      held `shouldReturn` [testHead cards]
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.revealBoostCards" _ _) -> pure ()
        _ -> error "expected a boost reveal prompt"
      answerQuestion (player1 tg) 0
      held `shouldReturn` []

  it "offers the oldest facedown boost first" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      cards <- stackEncounterDeck [Treacheries.advance, Treacheries.assault]
      resolve [DealBoost $ VillainRef vid, DealBoost $ VillainRef vid]
      withGame_ $ questionL .~ mempty
      pushAll [VillainMessage vid VillainFlipBoostCards]
      runGameMessages
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.revealBoostCards" (Just source) _) ->
          source `shouldBe` toTarget (testHead cards)
        _ -> error "expected the first dealt boost to be offered"

  it "waits for confirmation after boosts before placing scheme threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      before <- mainSchemeThreatNow
      withGame_ $ questionL .~ mempty
      L.runQueueT (L.capture $ L.villainSchemesWithoutBoostAgainst vid p) >>= pushAll
      runGameMessages
      mainSchemeThreatNow `shouldReturn` before
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.resolveEnemyScheme" _ _) -> pure ()
        _ -> error "expected a scheme resolution prompt"
      amount <- Villain.getModifiedScheme . toAttrs =<< getVillain
      answerQuestion p 0
      mainSchemeThreatNow `shouldReturn` (before + amount)

  it "waits for confirmation before applying an undefended attack's damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          damage = playerIdentityDamage . toAttrs <$> getPlayer p
          settle 0 = error "attack resolution prompt not reached"
          settle n = openQuestion >>= \case
            Just (_, QuestionLabel "$label.resolveEnemyAttack" _ _) -> pure ()
            Just (you, q) -> do
              let safe = headMay [i | (i,c) <- zip [0..] (choicesOf q), isNoResponse c || isNoDefender c]
              answerQuestion you (fromMaybe 0 safe)
              settle (n - 1)
            _ -> error "attack did not pause"
      changeForm p
      pass
      vid <- villainIdNow
      before <- damage
      withGame_ $ questionL .~ mempty
      pushAll [VillainMessage vid $ VillainAttacksWithoutBoost p]
      runGameMessages
      settle (30 :: Int)
      damage `shouldReturn` before
      answerQuestion p 0
      pass
      damage `shouldReturn` (before + 2)
