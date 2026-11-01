module Marvel.Scenario.AgeOfApocalypseEnSabahNur
  (AoaEnSabahNurScenario (..), aoaEnSabahNurScenario, enSabahNur, changeApocalypseForm, apocalypseInForm, placeApocalypsePower, ancientRitual, placeAncientRitualThreat) where

import Marvel.Prelude
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref ()
import Marvel.Scenario.Types
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait

newtype AoaEnSabahNurScenario = AoaEnSabahNurScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

aoaEnSabahNurScenario :: ScenarioCard AoaEnSabahNurScenario
aoaEnSabahNurScenario = scenario AoaEnSabahNurScenario "45184a" ("45184a", "45185a") ["45147", "45148"] [Set.AoaEnSabahNur]

instance RunMessage AoaEnSabahNurScenario where
  runMessage msg (AoaEnSabahNurScenario attrs) = AoaEnSabahNurScenario <$> runMessage msg attrs

enSabahNur :: HasGame m => m (Maybe VillainId)
enSabahNur = villainNamed "Apocalypse"

changeApocalypseForm :: ReverseQueue m => Text -> m ()
changeApocalypseForm form = enSabahNur >>= traverse_ (\vid -> cardSpecific vid "aoaChangeApocalypseForm" form)

apocalypseInForm :: HasGame m => Text -> m Bool
apocalypseInForm form = do
  current <- enSabahNur
  maybe (pure False) (enemyMatches (EnemyWithTrait $ case form of
    "Biomorph" -> Trait.Biomorph
    "Cyberpath" -> Trait.Cyberpath
    _ -> Trait.Giant) . EnemyVillainId) current

placeApocalypsePower :: ReverseQueue m => m ()
placeApocalypsePower = selectOne EncounterMainScheme >>= traverse_ (`placeCounters` 1)

ancientRitual :: HasGame m => m (Maybe SideSchemeId)
ancientRitual = selectOne $ SideSchemeIs Schemes.ancientRitual

placeAncientRitualThreat :: ReverseQueue m => Natural -> m ()
placeAncientRitualThreat n = ancientRitual >>= traverse_ (`placeThreat` n)
