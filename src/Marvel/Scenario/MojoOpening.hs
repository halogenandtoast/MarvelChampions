module Marvel.Scenario.MojoOpening where

import Data.HashSet qualified as HashSet
import Marvel.Card ()
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source ()
import Marvel.Message.Lifted
import Marvel.Prelude
import Marvel.Scenario.Types
import System.Random.Shuffle (shuffleM)

newtype MojoOpeningScenario = MojoOpeningScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

mojoMagogScenario, mojoSpiralScenario :: ScenarioCard MojoOpeningScenario
mojoMagogScenario = scenario MojoOpeningScenario "39001a" ("39001a", "39001b") ["39002b"] [Set.Magog]
mojoSpiralScenario = scenario MojoOpeningScenario "39012a" ("39012a", "39013a") ["39015b"] [Set.Spiral]

genreSets :: [Set.EncounterSet]
genreSets = [Set.Crime, Set.Fantasy, Set.Horror, Set.SciFi, Set.Sitcom, Set.Western]

instance RunMessage MojoOpeningScenario where
  runMessage msg (MojoOpeningScenario a) = MojoOpeningScenario <$> case msg of
    StartScenario -> do
      let suppliedOther = any (\set -> set `notElem` genreSets && set /= Set.LongshotSet && set `HashSet.member` scenarioEncounterSets a) Set.modularSets
          required = if scenarioId a == "39001a" then if suppliedOther then 0 else 1 else 3
          selected = filter (`HashSet.member` scenarioEncounterSets a) genreSets
      rest <- shuffleM $ filter (`notElem` selected) genreSets
      let chosen = take required (selected <> rest)
          sets = HashSet.union (HashSet.fromList chosen) (HashSet.difference (scenarioEncounterSets a) (HashSet.fromList genreSets))
      runMessage StartScenario a {scenarioEncounterSets = sets}
    _ -> runMessage msg a
