module Marvel.Scenario.AgeOfApocalypseApocalypse
  (AoaApocalypseScenario (..), aoaApocalypseScenario, apocalypse, apocalypseScalarHp, revealRandomPrelate) where

import Marvel.Prelude
import Data.HashMap.Strict ()
import Marvel.Card
import Marvel.Difficulty ()
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Villain.Cards qualified as Villains
import System.Random.Shuffle (shuffleM)

newtype AoaApocalypseScenario = AoaApocalypseScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

aoaApocalypseScenario :: ScenarioCard AoaApocalypseScenario
aoaApocalypseScenario = scenario AoaApocalypseScenario "45101b" ("45101b", "45102a") ["45103"] [Set.AoaApocalypse, Set.AoaPrelates]

apocalypse :: HasGame m => m (Maybe VillainId)
apocalypse = villainNamed "Apocalypse"

apocalypseScalarHp :: HasGame m => m Natural
apocalypseScalarHp = do
  current <- apocalypse
  case current of
    Nothing -> pure 9
    Just vid -> getsGame $ maybe 9 (\v -> case toCardCode v of
      "45101a" -> 8
      "45101b" -> 9
      "45102a" -> 10
      "45102b" -> 11
      _ -> 9) . (`abilityCardDef` VillainRef vid)

revealRandomPrelate :: ReverseQueue m => IdentityId -> m ()
revealRandomPrelate = cardSpecific GameRef "aoaRevealRandomPrelate"

instance RunMessage AoaApocalypseScenario where
  runMessage msg s@(AoaApocalypseScenario attrs) = runQueueT $ case msg of
    StartScenario -> do
      started <- runMessage msg attrs
      unused <- traverse genEncounterCard [def | def <- [Villains.apocalypse1, Villains.apocalypse3], toBaseCardCode (toCardCode def) `notElem` map toBaseCardCode (scenarioVillains attrs)]
      let withheld card = cardMatch (CardWithTrait Trait.Prelate) card || sameCard (getCardDef card) Schemes.theTyrantsThrone
          setAsideCards = filter withheld (scenarioEncounterDeck started)
      pure $ AoaApocalypseScenario started
        {scenarioEncounterDeck = filter (not . withheld) (scenarioEncounterDeck started),
         scenarioSetAsideCards = scenarioSetAsideCards started <> map EncounterCard (setAsideCards <> unused)}
    CardSpecific GameRef "aoaRevealRandomPrelate" (fromJSON -> Success you) -> do
      available <- shuffleM [card | EncounterCard card <- scenarioSetAsideCards attrs, cardMatch (CardWithType MinionType <> CardWithTrait Trait.Prelate) card]
      for_ (headMay available) $ revealEncounterCardFrom W.RevealedFromSetAside you
      pure s
    CardSpecific GameRef "aoaApocalypseStageAdvanced" (fromJSON -> Success code) -> pure $ AoaApocalypseScenario attrs
      {scenarioSetAsideCards = filter ((/= toBaseCardCode code) . toBaseCardCode . toCardCode) $ scenarioSetAsideCards attrs}
    ResolveVillainDefeat _ -> s <$ gameOver Won
    _ -> AoaApocalypseScenario <$> runMessage msg attrs
