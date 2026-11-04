module Marvel.Scenario.FearNoEvil where

import Marvel.Prelude
import Data.HashSet qualified as HashSet
import Marvel.Card
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet qualified as Set
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.FearNoEvil.Kingpin qualified as Kingpin
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types

newtype FearNoEvilScenario = FearNoEvilScenario (Attrs Scenario)
  deriving newtype (Show,Eq,ToJSON,FromJSON)
  deriving anyclass IsScenario
-- Each standalone scenario chooses its underling before generating its encounter deck.
-- The campaign will supply its recorded assignment through the same continuation.
fearNoEvilScenarios :: [ScenarioCard FearNoEvilScenario]
fearNoEvilScenarios = kingpinScenario : [scenario FearNoEvilScenario code ("60086","60087") [main] [set] | (code,main,set) <-
  [("60121a","60121b",Set.FneArtMuseumHeist),("60128a","60128b",Set.FneTheGetaway),("60142a","60142b",Set.FneTheRaftBreakout),("60151a","60151b",Set.FneStopThePresses),("60134a","60134b",Set.FneProtectionRacket)]]
kingpinScenario :: ScenarioCard FearNoEvilScenario
kingpinScenario = scenario FearNoEvilScenario "60161a" ("60159a","60160a") ["60161b","60162b"] [Set.FneKingpin]

underlings :: [(CardCode,CardCode,Set.EncounterSet)]
underlings = [("60065","60066",Set.FneBullseye),("60076","60077",Set.FneElectro),("60086","60087",Set.FneHammerhead),("60097","60098",Set.FnePurpleMan),("60110a","60111a",Set.FneTyphoidMary)]
instance RunMessage FearNoEvilScenario where
  runMessage msg e@(FearNoEvilScenario a) = runQueueT $ case msg of
    StartScenario | scenarioId a == "60161a" -> FearNoEvilScenario <$> runMessage StartScenario a {scenarioEncounterSets=foldr HashSet.delete (scenarioEncounterSets a) [Set.Standard,Set.StandardII,Set.StandardIII]}
    CardSpecific GameRef "kingpinNemesis" (fromJSON -> Success (you,initial)) -> Kingpin.revealKingpinNemesis you initial >> pure e
    CardSpecific GameRef "kingpinNemesisEntered" (fromJSON -> Success (you,cid :: CardId)) -> do
      entered <- selectAny $ MinionWithId $ MinionId $ unCardId cid
      unless entered $ Kingpin.findUnderling you
      pure e
    StartScenario -> do
      first <- selectJust FirstPlayer
      chooseOne first [cardLabeled normal $ cardSpecific GameRef "fneChooseUnderling" normal | (normal,_,_) <- underlings]
      pure e
    CardSpecific GameRef "fneChooseUnderling" (fromJSON -> Success normal) -> do
      let chosen = headMay [entry | entry@(code,_,_) <- underlings,code == normal]
      case chosen of
        Nothing -> pure e
        Just (_,hard,set) -> do
          marySide <- if normal == "60110a" then getRandomR (False,True) else pure False
          let initial = if scenarioDifficulty a == Expert then hard else normal
              villain = if marySide then flipCardCode initial else initial
              attrs = a {scenarioVillains=[villain],scenarioEncounterSets=HashSet.insert set (scenarioEncounterSets a)}
          spine <- if normal == "60065" then traverse genEncounterCard [Cards.street60068a] else pure []
          businesses <- if scenarioId a == "60134a" then traverse genEncounterCard [Cards.street60134b,Cards.street60135b,Cards.street60136b,Cards.street60137b,Cards.street60138b] else pure []
          attrs' <- runMessage StartScenario attrs {scenarioSetAsideCards=scenarioSetAsideCards attrs <> map EncounterCard (spine <> businesses)}
          when (normal == "60110a") $ do
            first <- selectJust FirstPlayer
            trust <- genEncounterCard Cards.street60113a
            revealEncounterCard first trust
            for_ [card | card <- scenarioEncounterDeck attrs',toCardCode card == "60112"] $ revealEncounterCard first
          pure $ FearNoEvilScenario $ if normal == "60110a"
            then attrs' {scenarioEncounterDeck=filter ((/= "60112") . toCardCode) $ scenarioEncounterDeck attrs'}
            else attrs'
    _ -> FearNoEvilScenario <$> runMessage msg a
