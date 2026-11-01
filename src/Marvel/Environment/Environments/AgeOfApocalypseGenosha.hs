module Marvel.Environment.Environments.AgeOfApocalypseGenosha where

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

newtype AoaGenoshaEnvironment = AoaGenoshaEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

genosha :: EnvironmentCard AoaGenoshaEnvironment
genosha = environment AoaGenoshaEnvironment Cards.genosha

instance HasModifiersFor AoaGenoshaEnvironment where
  getModifiersFor _ (VillainRef _) _ = pure [KeywordModifier Steady]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaGenoshaEnvironment where
  runMessage msg e@(AoaGenoshaEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid (RevealEnvironment _ _) | eid == environmentId attrs -> e <$ replaceOtherSettings eid
    _ -> AoaGenoshaEnvironment <$> runMessage msg attrs
