module Marvel.Scenario where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Message
import Marvel.Scenario.Attrs hiding (scenarioAccelerationTokens)
import Marvel.Scenario.Attrs qualified as Attrs
import Text.Show qualified

data Scenario = forall a. IsScenario a => Scenario a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a, Entity a, EntityAttrs a ~ ScenarioAttrs, EntityId a ~ CardCode) => IsScenario a

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance Eq Scenario where
  (Scenario (a :: a)) == (Scenario (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Scenario where
  parseJSON = withObject "Scenario" $ \o -> do
    cardCode <- o .: "scenarioId"
    withScenarioCardCode cardCode $ \(_ :: ScenarioCard a) -> Scenario <$> parseJSON @a (Object o)

withScenarioCardCode
  :: CardCode
  -> (forall a. IsScenario a => ScenarioCard a -> r)
  -> r
withScenarioCardCode cCode f =
  case lookup cCode allScenarios of
    Nothing -> error "invalid minion"
    Just (SomeScenarioCard a) -> f a

data SomeScenarioCard = forall a. IsScenario a => SomeScenarioCard (ScenarioCard a)

liftScenarioCard :: (forall a . ScenarioCard a -> b) -> SomeScenarioCard -> b
liftScenarioCard f (SomeScenarioCard a) = f a

someScenarioCardCode :: SomeScenarioCard -> CardCode
someScenarioCardCode = liftScenarioCard cbCardCode

allScenarios :: HashMap CardCode SomeScenarioCard
allScenarios =
  fromList $
    map
      (toFst someScenarioCardCode)
      [ SomeScenarioCard rhinoScenario
      , SomeScenarioCard klawScenario
      ]

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario cCode = case lookup cCode allScenarios of
  Just (SomeScenarioCard a) -> Just $ Scenario $ cbCardBuilder a ()
  Nothing -> Nothing

-- allScenarios :: HashMap CardCode Scenario
-- allScenarios = fromList [("01094", RhinoScenario' rhinoScenario), ("01113", KlawScenario' klawScenario)]
--
newtype RhinoScenario = RhinoScenario ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage RhinoScenario where
  runMessage msg (RhinoScenario a) = RhinoScenario <$> runMessage msg a

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario a) = KlawScenario <$> runMessage msg a

rhinoScenario :: ScenarioCard RhinoScenario
rhinoScenario = scenario
  RhinoScenario
  "01094"
  ["01094"]
  ["01097"]
  [EncounterSet.Rhino, EncounterSet.BombScare, EncounterSet.Standard]

klawScenario :: ScenarioCard KlawScenario
klawScenario = scenario
  KlawScenario
  "01113"
  ["01113"]
  ["01116", "01117"]
  [EncounterSet.Klaw, EncounterSet.MastersOfEvil, EncounterSet.Standard]

type ScenarioCard a = CardBuilder () a

instance Entity Scenario where
  type EntityId Scenario = CardCode
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs (Scenario a) = toAttrs a

getScenarioDifficulty :: Scenario -> Difficulty
getScenarioDifficulty = scenarioDifficulty . toAttrs

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Attrs.scenarioAccelerationTokens . toAttrs
