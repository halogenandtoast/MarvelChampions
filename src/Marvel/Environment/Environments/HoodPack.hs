module Marvel.Environment.Environments.HoodPack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

newtype HoodEnvironment = HoodEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hood24049, hood24049a, hood24049b, hood24060, hood24061, hood24062, hood24063 :: EnvironmentCard HoodEnvironment
hood24049 = environment HoodEnvironment Cards.hood24049
hood24049a = environment HoodEnvironment Cards.hood24049a
hood24049b = environment HoodEnvironment Cards.hood24049b
hood24060 = environment HoodEnvironment Cards.hood24060
hood24061 = environment HoodEnvironment Cards.hood24061
hood24062 = environment HoodEnvironment Cards.hood24062
hood24063 = environment HoodEnvironment Cards.hood24063

instance HasModifiersFor HoodEnvironment where
  getModifiersFor _ target (HoodEnvironment a) = do
    difficulty <- getDifficulty
    pure $ case toCardCode a of
      code | code `elem` ["24049", "24049a", "24049b"] ->
        [KeywordModifier Steady | enemy target && (difficulty == Expert || villain target)]
        <> [AccelerationIconModifier 1 | target == toRef a && difficulty == Expert]
      "24060" -> [AttackModifier 1 | character target]
      "24061" -> [AccelerationIconModifier 1 | enemy target] <> [ThwartModifier 1 | friendly target]
      "24062" -> [KeywordModifier (Retaliate 1) | character target]
      "24063" -> [KeywordModifier Steady | character target]
      _ -> []
   where
    friendly = \case IdentityRef _ -> True; AllyRef _ -> True; _ -> False
    enemy = \case VillainRef _ -> True; MinionRef _ -> True; _ -> False
    villain = \case VillainRef _ -> True; _ -> False
    character ref = friendly ref || enemy ref

instance RunMessage HoodEnvironment where
  runMessage msg e@(HoodEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs, toCardCode attrs `elem` ["24049", "24049a", "24049b"] -> do
      difficulty <- getDifficulty
      pure $ HoodEnvironment attrs {environmentCardDef = if difficulty == Expert then Cards.hood24049b else Cards.hood24049a}
    EnvironmentMessage eid (RevealEnvironment _ _) | eid == environmentId attrs -> do
      if toCardCode attrs `elem` ["24049", "24049a", "24049b"] then do
        difficulty <- getDifficulty
        pure $ HoodEnvironment attrs {environmentCardDef = if difficulty == Expert then Cards.hood24049b else Cards.hood24049a}
      else do
        settings <- selectList (EnvironmentWithTrait Trait.Setting)
        traverse_ removeFromPlay (filter (/= eid) settings)
        HoodEnvironment <$> runMessage msg attrs
    _ -> HoodEnvironment <$> runMessage msg attrs
