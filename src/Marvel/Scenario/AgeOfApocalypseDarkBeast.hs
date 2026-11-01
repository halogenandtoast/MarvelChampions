module Marvel.Scenario.AgeOfApocalypseDarkBeast (AoaDarkBeastScenario (..), aoaDarkBeastScenario) where

import Marvel.Prelude
import Data.HashSet qualified as HashSet
import Marvel.Card
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)

newtype AoaDarkBeastScenario = AoaDarkBeastScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

aoaDarkBeastScenario :: ScenarioCard AoaDarkBeastScenario
aoaDarkBeastScenario = scenario AoaDarkBeastScenario "45118" ("45118", "45119") ["45121"]
  [Set.AoaDarkBeast, Set.AoaBlueMoon, Set.AoaGenosha, Set.AoaSavageLand]

settingSets :: [Set.EncounterSet]
settingSets = [Set.AoaBlueMoon, Set.AoaGenosha, Set.AoaSavageLand]

instance RunMessage AoaDarkBeastScenario where
  runMessage msg e@(AoaDarkBeastScenario attrs) = runQueueT $ case msg of
    StartScenario -> do
      aside <- concatMapM gatherEncounterSet settingSets
      AoaDarkBeastScenario <$> runMessage msg attrs
        {scenarioEncounterSets = scenarioEncounterSets attrs `HashSet.difference` HashSet.fromList settingSets
        ,scenarioSetAsideCards = scenarioSetAsideCards attrs <> map EncounterCard aside}
    CardSpecific GameRef "aoaRecruitSetting" _ -> do
      chosen <- shuffleM [card | EncounterCard card <- scenarioSetAsideCards attrs, cdCardType (getCardDef card) == EnvironmentType,
        cdEncounterSet (getCardDef card) `elem` map Just settingSets]
      for_ (headMay chosen) \environment -> do
        first <- selectJust FirstPlayer
        let recruited = [card | EncounterCard card <- scenarioSetAsideCards attrs, cdEncounterSet (getCardDef card) == cdEncounterSet (getCardDef environment)]
        revealEncounterCardFrom W.RevealedFromSetAside first environment
        shuffleIntoEncounterDeck (filter (/= environment) recruited)
      pure e
    _ -> AoaDarkBeastScenario <$> runMessage msg attrs
