module Marvel.Scenario.Mojo where

import Marvel.Prelude
import Data.HashSet qualified as HashSet
import Data.List (nub)
import Marvel.Card
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Window qualified as W
import Control.Monad (replicateM_)
import System.Random.Shuffle (shuffleM)

newtype MojoScenario = MojoScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

mojoScenario :: ScenarioCard MojoScenario
mojoScenario = scenario MojoScenario "39022" ("39022", "39023") ["39025b"] [Set.Mojo]

mojoGenres :: [Set.EncounterSet]
mojoGenres = [Set.Crime, Set.Fantasy, Set.Horror, Set.SciFi, Set.Sitcom, Set.Western]

instance RunMessage MojoScenario where
  runMessage msg e@(MojoScenario a) = runQueueT $ case msg of
    StartScenario -> do
      count <- getPlayerCount
      let selected = filter (`elem` mojoGenres) $ HashSet.toList $ scenarioEncounterSets a
          candidates = if length selected < count + 1 then mojoGenres else selected
      cards <- concatMapM gatherEncounterSet candidates
      MojoScenario <$> runMessage StartScenario a
        { scenarioEncounterSets = scenarioEncounterSets a `HashSet.difference` HashSet.fromList mojoGenres
        , scenarioSetAsideCards = scenarioSetAsideCards a <> map EncounterCard cards
        }
    CardSpecific GameRef "mojoSelectGenres" (fromJSON -> Success (remaining :: Natural, chosen :: [Set.EncounterSet])) -> do
      let available = nub [set | EncounterCard c <- scenarioSetAsideCards a, Just set <- [cdEncounterSet $ getCardDef c], set `elem` mojoGenres, set `notElem` chosen]
      if remaining == 0 || null available
        then do
          cardSpecific GameRef "mojoRecruitGenre" False
          pure $ MojoScenario a {scenarioSetAsideCards = filter (\c -> cdEncounterSet (getCardDef c) `notElem` map Just mojoGenres || cdEncounterSet (getCardDef c) `elem` map Just chosen) (scenarioSetAsideCards a)}
        else if fromIntegral (length available) <= remaining
          then cardSpecific GameRef "mojoSelectGenres" (0 :: Natural, chosen <> available) >> pure e
          else do
            first <- selectJust FirstPlayer
            chooseOnePrompted first "$label.mojoChooseGenre"
              [cardLabeled (toCardCode c) $ cardSpecific GameRef "mojoSelectGenres" (remaining - 1, chosen <> [set])
              | set <- available
              , c <- take 1 [c | EncounterCard c <- scenarioSetAsideCards a, cdEncounterSet (getCardDef c) == Just set, cdCardType (getCardDef c) == EnvironmentType]]
            pure e
    CardSpecific GameRef "mojoRecruitGenre" (fromJSON -> Success stopped) -> do
      let cards = [c | EncounterCard c <- scenarioSetAsideCards a, cdEncounterSet (getCardDef c) `elem` map Just mojoGenres]
      sets <- shuffleM $ nub $ map (cdEncounterSet . getCardDef) cards
      case sets of
        [] -> gameOver Lost >> pure e
        set : _ -> do
          first <- selectJust FirstPlayer
          let recruited = filter ((== set) . cdEncounterSet . getCardDef) cards
              environments = filter ((== EnvironmentType) . cdCardType . getCardDef) recruited
              rest = filter ((/= EnvironmentType) . cdCardType . getCardDef) recruited
          traverse_ (revealEncounterCardFrom W.RevealedFromSetAside first) environments
          if stopped
            then do
              ordered <- shuffleM rest
              putOnTopOfEncounterDeck ordered
              replicateM_ 2 (dealEncounterCard first)
            else shuffleIntoEncounterDeck rest
          pure $ MojoScenario a {scenarioSetAsideCards = filter (\c -> cdEncounterSet (getCardDef c) /= set) (scenarioSetAsideCards a)}
    CardSpecific GameRef "mojoEncounterDeckReset" _ -> do
      wheels <- selectList (EnvironmentIs Environments.mojo39026a)
      unless (null wheels) do
        let remaining = any (\c -> cdEncounterSet (getCardDef c) `elem` map Just mojoGenres) (scenarioSetAsideCards a)
        if remaining then traverse_ flipEnvironment wheels else gameOver Lost
      pure e
    _ -> MojoScenario <$> runMessage msg a
