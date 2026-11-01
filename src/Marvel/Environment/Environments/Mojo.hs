module Marvel.Environment.Environments.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Ref

newtype MojoEnvironment = MojoEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39026a, mojo39026b :: EnvironmentCard MojoEnvironment
mojo39026a = environment MojoEnvironment Cards.mojo39026a
mojo39026b = environment MojoEnvironment Cards.mojo39026b

instance RunMessage MojoEnvironment where
  runMessage msg e@(MojoEnvironment a) = runQueueT $ case msg of
    VillainPhaseStep StepDealEncounterCards | toCardCode a == "39026b" -> do
      cardSpecific GameRef "mojoRecruitGenre" True
      flipEnvironment (environmentId a)
      pure e
    _ -> MojoEnvironment <$> runMessage msg a
