module Marvel.Scenario.AgeOfApocalypseFourHorsemen
  (AoaFourHorsemenScenario (..), aoaFourHorsemenScenario, horsemanHitPoints, horsemanResponse, moveHorsemanCounter, horsemanHasMinimumHitPoints, findHorseman) where

import Marvel.Prelude
import Marvel.Id
import Data.List (elemIndex)
import Data.HashMap.Strict ()
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card
import Marvel.Damage ()
import Marvel.Difficulty
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Villain.Types ()
import System.Random.Shuffle (shuffleM)

newtype AoaFourHorsemenScenario = AoaFourHorsemenScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

aoaFourHorsemenScenario :: ScenarioCard AoaFourHorsemenScenario
aoaFourHorsemenScenario = ScenarioCard "45081a" $ \setup ->
  let AoaFourHorsemenScenario attrs = buildScenario
        (scenario AoaFourHorsemenScenario "45081a" ("45081a", "45081b") ["45085"] [Set.AoaFourHorsemen]) setup
      codes = if setup.difficulty == Expert then ["45081b", "45082b", "45083b", "45084b"] else ["45081a", "45082a", "45083a", "45084a"]
   in AoaFourHorsemenScenario attrs {scenarioVillains = codes}

-- Golden Horse and Metal Wings change the hit points used by these card checks,
-- while leaving the physical dial at zero.
horsemanHitPoints :: HasGame m => VillainId -> m Natural
horsemanHitPoints vid = do
  hp <- getVillainHitPoints vid
  protected <- horsemanHasMinimumHitPoints vid
  pure $ max hp (if protected then 1 else 0)

horsemanHasMinimumHitPoints :: HasGame m => VillainId -> m Bool
horsemanHasMinimumHitPoints vid = do
  horse <- selectAny $ AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentIs Attachments.goldenHorse
  wings <- selectAny $ AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentIs Attachments.metalWings
  pure $ horse || wings

-- The row is persisted in scenarioVillains; moving is relative to the CURRENT
-- active counter, even when a named inactive villain just activated.
moveHorsemanCounter :: ReverseQueue m => m ()
moveHorsemanCounter = cardSpecific GameRef "aoaHorsemenMoveCounter" ()

findHorseman :: HasGame m => Text -> m (Maybe VillainId)
findHorseman = villainNamed

horsemanResponse :: ReverseQueue m => VillainId -> IdentityId -> m ()
horsemanResponse vid you = cardSpecific vid "aoaHorsemanResponse" you

instance RunMessage AoaFourHorsemenScenario where
  runMessage msg s@(AoaFourHorsemenScenario attrs) = runQueueT $ case msg of
    StartScenario -> do
      shuffled <- shuffleM (scenarioVillains attrs)
      AoaFourHorsemenScenario <$> runMessage msg attrs {scenarioVillains = shuffled}
    CardSpecific GameRef "aoaHorsemenMoveCounter" _ -> do
      villains <- selectList AnyVillain
      row <- for (scenarioVillains attrs) $ \code -> filterM (\vid -> getsGame $ maybe False ((== code) . toCardCode) . (`abilityCardDef` VillainRef vid)) villains
      active <- selectOne ActiveVillain
      let ordered = concat row
          next = case active >>= (`elemIndex` ordered) of
            Just i | not (null ordered) -> Just $ ordered !! ((i + 1) `mod` length ordered)
            _ -> headMay ordered
      for_ next setActiveVillain
      pure s
    CardSpecific GameRef "aoaHorsemenCheckVictory" _ -> do
      villains <- selectList AnyVillain
      whenM (allM (fmap (== 0) . horsemanHitPoints) villains) $ gameOver Won
      pure s
    ResolveVillainDefeat _ -> s <$ gameOver Won
    _ -> AoaFourHorsemenScenario <$> runMessage msg attrs
