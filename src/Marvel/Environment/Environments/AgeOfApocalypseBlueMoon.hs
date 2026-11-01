module Marvel.Environment.Environments.AgeOfApocalypseBlueMoon where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Keyword
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

newtype AoaBlueMoonEnvironment = AoaBlueMoonEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

blueAreaOfTheMoon :: EnvironmentCard AoaBlueMoonEnvironment
blueAreaOfTheMoon = environment AoaBlueMoonEnvironment Cards.blueAreaOfTheMoon

instance HasModifiersFor AoaBlueMoonEnvironment where
  getModifiersFor _ (MinionRef _) _ = pure [KeywordModifier Guard]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaBlueMoonEnvironment where
  runMessage msg e@(AoaBlueMoonEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid (RevealEnvironment _ _) | eid == environmentId attrs -> e <$ replaceOtherSettings eid
    _ -> AoaBlueMoonEnvironment <$> runMessage msg attrs
