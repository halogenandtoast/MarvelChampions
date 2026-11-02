module Marvel.Scenario.TricksterTakeover where

import Marvel.Prelude
import Marvel.Card
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Trait qualified as Trait
import Marvel.Phase
import System.Random.Shuffle (shuffleM)

newtype TricksterScenario = TricksterScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

enchantressScenario, godOfLiesScenario :: ScenarioCard TricksterScenario
enchantressScenario = scenario TricksterScenario "55001" ("55001", "55002") ["55004b", "55005b"] [Set.EnchantressScenario]
godOfLiesScenario = scenario TricksterScenario "55027a" ("55029a", "55029a") ["55033b"] [Set.GodOfLies]

instance RunMessage TricksterScenario where
  runMessage msg e@(TricksterScenario a) = runQueueT $ case msg of
    StartScenario | scenarioId a == "55027a" -> do
      n <- getPlayerCount
      villains <- shuffleM ["55029a", "55030a", "55031a", "55032a"]
      TricksterScenario <$> runMessage msg a
        {scenarioVillains = take 1 villains, scenarioTrickster = Just $ TricksterState (20 * fromIntegral n) False 0 False}
    CardSpecific GameRef "ttCharm" (fromJSON -> Success you) -> do
      enslaved <- enthralled you
      if enslaved then mainThreat GameRef 1 else do
        attachments <- selectList (AttachmentWithTrait Trait.Enchantment)
        for_ attachments $ \aid -> cardSpecific aid "ttCharmFor" you
      pure e
    CardSpecific GameRef "ttSynergy" (fromJSON -> Success (code :: CardCode, n :: Natural)) -> do
      environments <- selectList (EnvironmentWithTrait Trait.Synergy)
      for_ environments $ \eid -> cardSpecific eid "ttAddNamedSynergy" (code, n)
      pure e
    CardSpecific GameRef "ttLokiDamage" (fromJSON -> Success (n :: Natural)) -> do
      count <- getPlayerCount
      case scenarioTrickster a of
        Nothing -> pure e
        Just state -> do
          let remaining = subtractNatural n $ lokiHitPoints state
              flipNow = remaining <= 10 * fromIntegral count && not (lokiFlipped state)
          when (remaining == 0) $ gameOver Won
          when (flipNow && remaining > 0) $ cardSpecific GameRef "ttFocus" ()
          pure $ TricksterScenario a {scenarioTrickster = Just state {lokiHitPoints = remaining, lokiFlipped = lokiFlipped state || flipNow}}
    CardSpecific GameRef "ttFocus" _ -> do
      first <- selectJust FirstPlayer
      focuses <- selectList (AttachmentIs Cards.tt55034a)
      if null focuses
        then revealEncounterCard first =<< genEncounterCard Cards.tt55034a
        else traverse_ (\aid -> cardSpecific aid "ttTotalFocus" ()) focuses
      pure e
    CardSpecific GameRef "ttWorldsThreat" _ -> do
      case scenarioTrickster a of
        Nothing -> pure e
        Just state -> do
          let threat = worldsCollideThreat state + 1
              complete = threat >= 2
          when complete $ do
            phase <- getPhase
            if phase == PlayerPhase then pure () else gameOver Lost
          pure $ TricksterScenario a {scenarioTrickster = Just state {worldsCollideThreat = threat, worldsCollideComplete = complete}}
    AdvanceMainScheme _ | scenarioId a == "55027a" -> pure e
    EndPhase PlayerPhase | maybe False worldsCollideComplete (scenarioTrickster a) -> gameOver Lost >> pure e
    CardSpecific GameRef "ttSwapAvatar" (fromJSON -> Success (reset :: Bool)) -> do
      villains <- selectOne ActiveVillain
      traverse_ (\v -> cardSpecific v "ttSwapAvatar" reset) villains
      pure e
    _ -> TricksterScenario <$> runMessage msg a
