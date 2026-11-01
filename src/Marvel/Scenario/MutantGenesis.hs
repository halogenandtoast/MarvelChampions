module Marvel.Scenario.MutantGenesis where

import Marvel.Prelude
import Marvel.Card
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Villain.Cards qualified as Villains
import System.Random.Shuffle (shuffleM)

newtype MutantGenesisScenario = MutantGenesisScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

mgSabretoothScenario, mgProjectWideawakeScenario, mgMasterMoldScenario, mgMansionAttackScenario, mgMagnetoScenario :: ScenarioCard MutantGenesisScenario
mgSabretoothScenario = scenario MutantGenesisScenario "32060" ("32060", "32061") ["32063", "32064"] [Set.Sabretooth]
mgProjectWideawakeScenario = scenario MutantGenesisScenario "32084" ("32084", "32085") ["32087"] [Set.ProjectWideawake, Set.ZeroTolerance]
mgMasterMoldScenario = scenario MutantGenesisScenario "32109" ("32109", "32110") ["32112", "32113"] [Set.MasterMold, Set.Sentinels]
mgMansionAttackScenario = scenario MutantGenesisScenario "32121a" ("32121a", "32121b") ["32125", "32126", "32127", "32128", "32129"] [Set.MansionAttack, Set.Brotherhood]
mgMagnetoScenario = scenario MutantGenesisScenario "32138" ("32138", "32139") ["32141", "32142", "32143"] [Set.Magneto]

instance RunMessage MutantGenesisScenario where
  runMessage msg (MutantGenesisScenario a) = MutantGenesisScenario <$> case msg of
    StartScenario | scenarioId a == "32121a" -> do
      let defs = if scenarioDifficulty a == Expert
            then [Villains.mgAvalancheB, Villains.mgBlobB, Villains.mgPyroB, Villains.mgToadB]
            else [Villains.mgAvalancheA, Villains.mgBlobA, Villains.mgPyroA, Villains.mgToadA]
      villains <- shuffleM =<< traverse genEncounterCard defs
      schemes <- shuffleM ["32126", "32127", "32128", "32129"]
      case villains of
        first : rest -> runMessage StartScenario a
          { scenarioVillains = [toCardCode first]
          , scenarioMainSchemes = "32125" : schemes
          , scenarioSetAsideCards = scenarioSetAsideCards a <> map EncounterCard rest
          , scenarioVictoryCondition = Just $ if scenarioDifficulty a == Expert then 3 else 2
          }
        [] -> pure a
    ResolveVillainDefeat vid | scenarioId a == "32121a" -> runQueueT do
      def <- getsGame (`abilityCardDef` VillainRef vid)
      for_ def (\d -> addCardToVictoryDisplay =<< genEncounterCard d)
      victory <- getVictoryDisplay
      let defeated = 1 + length [c | c <- victory, cdCardType (getCardDef c) == VillainType]
      if fromIntegral defeated >= fromMaybe 2 (scenarioVictoryCondition a)
        then gameOver Won >> pure a
        else case [c | EncounterCard c <- scenarioSetAsideCards a, cdCardType (getCardDef c) == VillainType] of
          next : _ -> do
            -- A new Brotherhood member enters play; none of the defeated villain's
            -- status cards or attachments transfer to that different character.
            removeFromPlay (VillainRef vid)
            players <- getPlayers
            traverse_ dealEncounterCard players
            addVillain (toCardCode next)
            cardSpecific GameRef "mgNewMansionVillain" (toCardCode next)
            pure a {scenarioSetAsideCards = filter (/= EncounterCard next) (scenarioSetAsideCards a)}
          [] -> gameOver Won >> pure a
    _ -> runMessage msg a
