module Marvel.NextEvolution.SinisterScenario where

import Marvel.Prelude
import Marvel.Card
import Marvel.EncounterCard (gatherEncounterSets)
import Data.HashSet qualified as HashSet
import Marvel.EncounterSet qualified as Set
import Marvel.Message.Lifted
import Marvel.Scenario.Types
import System.Random.Shuffle (shuffleM)

-- | Sinister's three power sets stay out of the encounter deck during setup,
-- regardless of their Setup keywords. One stage 2 is removed, the other two form
-- a random main-scheme order. Every choice uses the stored game generator.
prepareMisterSinisterScenario :: ReverseQueue m => Attrs Scenario -> m (Attrs Scenario)
prepareMisterSinisterScenario attrs = do
  let sets = [Set.FlightSet, Set.SuperStrengthSet, Set.TelepathySet]
  powers <- gatherEncounterSets (HashSet.fromList sets)
  stages <- shuffleM ["40140", "40141", "40142"]
  pure attrs
    { scenarioEncounterSets = HashSet.difference (scenarioEncounterSets attrs) (HashSet.fromList sets)
    , scenarioSetAsideCards = scenarioSetAsideCards attrs <> map EncounterCard powers
    , scenarioMainSchemes = "40139" : drop 1 stages <> ["40143"]
    }
