module Marvel.Environment.Environments.AgeOfApocalypseSavageLand where

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

newtype AoaSavageLandEnvironment = AoaSavageLandEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

theSavageLand :: EnvironmentCard AoaSavageLandEnvironment
theSavageLand = environment AoaSavageLandEnvironment Cards.theSavageLand

instance HasModifiersFor AoaSavageLandEnvironment where
  getModifiersFor _ (VillainRef _) _ = pure [KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaSavageLandEnvironment where
  runMessage msg e@(AoaSavageLandEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid (RevealEnvironment _ _) | eid == environmentId attrs -> e <$ replaceOtherSettings eid
    _ -> AoaSavageLandEnvironment <$> runMessage msg attrs
